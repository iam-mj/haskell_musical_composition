module Input.State where

import Music.Data
import Music.Utils
import Music.Show
import Input.Mappings
import Text.Parsec
import System.FilePath

-- new parser with the custom state
type MyParser = ParsecT String ParsingState IO

data ParsingState = PState {
    tracks   :: [(String, Track)], -- variables which were just defined
    melodies :: [(String, Music)]  -- variables which were given context & are ready to be played and saved
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
checkName :: String -> ParsingState -> Maybe String
checkName name state = let inTracks   = lookup name $ tracks state
                           inMelodies = lookup name $ melodies state
                           Just error = lookup uniqueNameErrKey errorMessages
                        in case inTracks of
                                Nothing -> case inMelodies of
                                            Nothing -> Nothing
                                            Just _  -> Just error
                                Just _  -> Just error

-- check that an index is positive
checkIndex :: Int -> Maybe String
checkIndex idx
    | idx > 0  = Nothing
    | otherwise = lookup negativeIdxErrKey errorMessages

addTrack :: String -> Track -> ParsingState -> IO (Maybe ParsingState)
addTrack name track state = 
    case checkName name state of
        Nothing  -> return $ Just $ state {tracks = (name, track) : tracks state}
        Just err -> putStrLn err >> return Nothing

addMusic :: String -> Music -> ParsingState -> IO (Maybe ParsingState)
addMusic name music state = 
    case checkName name state of
        Nothing  -> return $ Just $ state {melodies = (name, music) : melodies state}
        Just err -> putStrLn err >> return Nothing 

getTrack :: ParsingState -> String -> Either (IO ()) Track
getTrack state name = 
    let Just error = lookup noTracksErrKey errorMessages
    in case lookup name (tracks state) of
                        Nothing    -> Left $ putStrLn error
                        Just track -> Right track

getMusic :: ParsingState -> String -> Either (IO ()) Music
getMusic state name = 
    let Just error = lookup noMelodiesErrKey errorMessages
    in case lookup name (melodies state) of
                        Nothing    -> Left $ putStrLn error
                        Just music -> Right music

-- find in the state the value of the identifier provided
getValue :: ParsingState -> String -> Maybe (Either Track Music)
getValue state name = case lookup name (tracks state) of
                        Nothing     -> case lookup name (melodies state) of
                                            Nothing    -> Nothing
                                            Just music -> Just $ Right music
                        Just track  -> Just $ Left track

updateList :: String -> a -> [(String, a)] -> [(String, a)]
updateList name newValue [] = []
updateList name newValue ((name', value) : rest)
    | name' == name = (name', newValue) : rest
    | otherwise     = (name', value) : updateList name newValue rest

-- replace a track with a new value
updateTrack :: String -> Track -> ParsingState -> ParsingState
updateTrack name track state = state {tracks = updateList name track (tracks state)}

-- replace a melody with a new value
updateMusic :: String -> Music -> ParsingState -> ParsingState
updateMusic name music state = state {melodies = updateList name music (melodies state)}

printValue :: ParsingState -> String -> IO ()
printValue state name =
    let value      = getValue state name
        Just error = lookup noNameErrKey errorMessages
    in case value of
        Nothing        -> putStrLn error
        Just structure -> case structure of
                            Left track  -> print track
                            Right music -> print music

-- transpose either a track or a music with a number of semitones
transposeValue :: Maybe (Either Track Music) -> Int -> Either (IO ()) (Either Track Music)
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