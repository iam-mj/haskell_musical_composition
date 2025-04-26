module Input.State where

import Music.Data
import Music.Utils
import Music.Show
import Input.Messages
import Text.Parsec
import System.FilePath
import Control.Monad.Cont (liftIO)

-- types
type Name    = String              -- identifiers for the structures in the parser state
type PSValue = Either Track Music  -- structure recorded in the parser's state: track / melody (music)

data ParsingState = PState {
    tracks   :: [(Name, Track)], -- variables which were just defined
    melodies :: [(Name, Music)]  -- variables which were given context & are ready to be played and saved
} deriving Show

-- new parser with the custom state
type MyParser = ParsecT String ParsingState IO

emptyState :: ParsingState
emptyState = PState [] []

-- check that a track / music name is unique in the current state
checkName :: Name -> ParsingState -> Maybe Error
checkName name state = 
    let inTracks   = lookup name $ tracks state
        inMelodies = lookup name $ melodies state
        Just error = lookup NotUniqueName errorMessages
    in case inTracks of
            Nothing -> case inMelodies of
                        Nothing -> Nothing
                        Just _  -> Just $ error name
            Just _  -> Just $ error name

-- check that an index is positive
checkIndex :: Int -> Maybe Error
checkIndex idx
    | idx > 0   = Nothing
    | otherwise = let Just error = lookup NegativeIndex errorMessages
                  in Just $ error $ show idx

addTrack :: Name -> Track -> ParsingState -> Either Error ParsingState
addTrack name track state = 
    case checkName name state of
        Nothing  -> Right $ state {tracks = (name, track) : tracks state}
        Just err -> Left err

addMusic :: Name -> Music -> ParsingState -> Either Error ParsingState
addMusic name music state = 
    case checkName name state of
        Nothing  -> Right $ state {melodies = (name, music) : melodies state}
        Just err -> Left err

getTrack :: ParsingState -> Name -> Either Error Track
getTrack state name = 
    let Just error = lookup NoTrackName errorMessages
    in case lookup name (tracks state) of
            Nothing    -> Left $ error name
            Just track -> Right track

getMusic :: ParsingState -> Name -> Either Error Music
getMusic state name = 
    let Just error = lookup NoMelodyName errorMessages
    in case lookup name (melodies state) of
            Nothing    -> Left $ error name
            Just music -> Right music

-- find in the state the value of the identifier provided
getValue :: ParsingState -> Name -> Maybe PSValue
getValue state name = 
    case lookup name (tracks state) of
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
        Just error = lookup NoName errorMessages
    in case value of
        Nothing        -> putStrLn $ error name
        Just structure -> case structure of
                            Left track  -> print track
                            Right music -> print music

printState :: ParsingState -> IO ()
printState state = do
    printTracks   $ tracks state
    putStrLn "-----------------\n"
    printMelodies $ melodies state

printTracks :: [(Name, Track)] -> IO ()
printTracks tracks = putStrLn "Tracks\n" >> foldl accPrint (return ()) tracks
    where accPrint instr track = instr >> printTrack track 
          printTrack (name, track) = do
            putStrLn $ "Track " ++ name ++ ":" 
            print track
            putStrLn ""

printMelodies :: [(Name, Music)] -> IO ()
printMelodies melodies = putStrLn "Melodies\n" >> foldl accPrint (return ()) melodies
    where accPrint instr melody = instr >> printMelody melody
          printMelody (name, music) = do
            putStrLn $ "Melody " ++ name ++ ":" 
            print music
            putStrLn ""

-- transpose either a track or a music with a number of semitones
transposeValue :: Maybe PSValue -> Int -> Either Error PSValue
transposeValue value num = 
    let Just error = lookup NoName errorMessages
    in case value of 
        Nothing        -> Left $ error ""
        Just structure -> case structure of
                            Left track  -> Right $ Left $ transposeT track num
                            Right music -> Right $ Right $ transposeM music num 

validatePath :: FilePath -> Maybe String
validatePath file
    | takeExtension file == ".mid" = Nothing
    | otherwise                    = let Just error = lookup NotMidiFile errorMessages
                                     in Just $ error file