module Input.State where

import Music.Data
import Music.Utils
import Music.Show
import Input.Mappings
import Text.Parsec
import System.FilePath

-- types
type Name    = String              -- identifiers for the structures in the parser state
type Error   = String
type PSValue = Either Track Music 

-- new parser with the custom state
type MyParser = ParsecT String ParsingState IO

data ParsingState = PState {
    tracks   :: [(Name, Track)], -- variables which were just defined
    melodies :: [(Name, Music)]  -- variables which were given context & are ready to be played and saved
} deriving Show

-- errors
uniqueNameErrKey  = "NotUniqueName"     :: String
noTracksErrKey    = "NoTrackNameFound"  :: String
noMelodiesErrKey  = "NoMelodyNameFound" :: String
noNameErrKey      = "NoNameFound"       :: String
negativeIdxErrKey = "NegativeIndex"     :: String
notMidiFileErrKey = "NotAMidiFile"      :: String

emptyState :: ParsingState
emptyState = PState [] []

-- check that a track / music name is unique in the current state
checkName :: Name -> ParsingState -> Maybe Error
checkName name state = let inTracks   = lookup name $ tracks state
                           inMelodies = lookup name $ melodies state
                           Just error = lookup uniqueNameErrKey errorMessages
                        in case inTracks of
                                Nothing -> case inMelodies of
                                            Nothing -> Nothing
                                            Just _  -> Just error
                                Just _  -> Just error

-- check that an index is positive
checkIndex :: Int -> Maybe Error
checkIndex idx
    | idx > 0  = Nothing
    | otherwise = lookup negativeIdxErrKey errorMessages

addTrack :: Name -> Track -> ParsingState -> IO (Maybe ParsingState)
addTrack name track state = 
    case checkName name state of
        Nothing  -> return $ Just $ state {tracks = (name, track) : tracks state}
        Just err -> putStrLn err >> return Nothing

addMusic :: Name -> Music -> ParsingState -> IO (Maybe ParsingState)
addMusic name music state = 
    case checkName name state of
        Nothing  -> return $ Just $ state {melodies = (name, music) : melodies state}
        Just err -> putStrLn err >> return Nothing 

getTrack :: ParsingState -> Name -> Either (IO ()) Track
getTrack state name = 
    let Just error = lookup noTracksErrKey errorMessages
    in case lookup name (tracks state) of
                        Nothing    -> Left $ putStrLn error
                        Just track -> Right track

getMusic :: ParsingState -> Name -> Either (IO ()) Music
getMusic state name = 
    let Just error = lookup noMelodiesErrKey errorMessages
    in case lookup name (melodies state) of
                        Nothing    -> Left $ putStrLn error
                        Just music -> Right music

-- find in the state the value of the identifier provided
getValue :: ParsingState -> Name -> Maybe PSValue
getValue state name = case lookup name (tracks state) of
                        Nothing     -> case lookup name (melodies state) of
                                            Nothing    -> Nothing
                                            Just music -> Just $ Right music
                        Just track  -> Just $ Left track

updateList :: Name -> a -> [(Name, a)] -> [(Name, a)]
updateList name newValue [] = []
updateList name newValue ((name', value) : rest)
    | name' == name = (name', newValue) : rest
    | otherwise     = (name', value) : updateList name newValue rest

-- replace a track with a new value
updateTrack :: Name -> Track -> ParsingState -> ParsingState
updateTrack name track state = state {tracks = updateList name track (tracks state)}

-- replace a melody with a new value
updateMusic :: Name -> Music -> ParsingState -> ParsingState
updateMusic name music state = state {melodies = updateList name music (melodies state)}

printValue :: ParsingState -> Name -> IO ()
printValue state name =
    let value      = getValue state name
        Just error = lookup noNameErrKey errorMessages
    in case value of
        Nothing        -> putStrLn error
        Just structure -> case structure of
                            Left track  -> print track
                            Right music -> print music

printState :: ParsingState -> IO ()
printState state = do
    printTracks   $ tracks state
    putStrLn "-----------------\n"
    printMelodies $ melodies state

printTracks :: [(Name, Track)] -> IO ()
printTracks tracks = putStrLn "Tracks\n" >> foldl (\instr track -> instr >> printTrack track) (return ()) tracks
    where printTrack (name, track) = do
            putStrLn $ "Track " ++ name ++ ":" 
            print track
            putStrLn ""

printMelodies :: [(Name, Music)] -> IO ()
printMelodies melodies = putStrLn "Melodies\n" >> foldl (\instr melody -> instr >> printMusic melody) (return ()) melodies
    where printMusic (name, music) = do
            putStrLn $ "Melody " ++ name ++ ":" 
            print music
            putStrLn ""

-- transpose either a track or a music with a number of semitones
transposeValue :: Maybe PSValue -> Int -> Either (IO ()) PSValue
transposeValue value num = 
    let Just error = lookup noNameErrKey errorMessages
    in case value of 
        Nothing        -> Left $ putStrLn error
        Just structure -> case structure of
                            Left track  -> Right $ Left $ transposeT track num
                            Right music -> Right $ Right $ transposeM music num 

validatePath :: FilePath -> Maybe String
validatePath file
    | takeExtension file == ".mid" = Nothing
    | otherwise                    = lookup notMidiFileErrKey errorMessages