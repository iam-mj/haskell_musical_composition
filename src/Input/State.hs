module Input.State where

import Music.Data hiding (errorMessages, Error)
import Music.Utils
import Music.Show
import Input.Messages
import Text.Parsec
import Control.Monad.Cont (liftIO)
import Codec.Midi (Midi)

-- types
type Name    = String              -- identifiers for the structures in the parser state
type PSValue = Either Track Music  -- structure recorded in the parser's state: track / melody (music)

-- note: midi & modified names are the same as melodies names
-- note: if the music's name has a True in modified, we should remake the midi value

data ParsingState = PState {
    serverRuns :: Bool,            -- whether the local server for html-midi is already running
    tracks     :: [(Name, Track)], -- variables which were just defined
    melodies   :: [(Name, Music)], -- variables which were given context & are ready to be played and saved
    midi       :: [(Name, Midi)],  -- variables which have been transformed into midi
    modified   :: [(Name, Bool)]   -- have the music values changed since we registered the midi?
} deriving Show

-- new parser with the custom state
type MyParser = ParsecT String ParsingState IO

emptyState :: ParsingState
emptyState = PState False [] [] [] []

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

addMidi :: Name -> Midi -> ParsingState -> ParsingState
addMidi name newMidi state = state {midi = (name, newMidi) : midi state}

addModified :: Name -> ParsingState -> ParsingState
addModified name state = state {modified = (name, False) : modified state}

serverRunning :: ParsingState -> ParsingState
serverRunning state = state {serverRuns = True}

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
getValue :: ParsingState -> Name -> Either Error PSValue
getValue state name = 
    let Just error = lookup NoName errorMessages
    in case lookup name (tracks state) of
            Nothing     -> case lookup name (melodies state) of
                                Nothing    -> Left $ error name
                                Just music -> Right $ Right music
            Just track  -> Right $ Left track

-- replace a track with a new value
updateTrack :: Name -> Track -> ParsingState -> ParsingState
updateTrack name track state = state {tracks = updateList name track (tracks state)}

-- replace a melody with a new value
updateMusic :: Name -> Music -> ParsingState -> ParsingState
updateMusic name music state = state {melodies = updateList name music (melodies state)}

-- replace a midi value 
updateMidi :: Name -> Midi -> ParsingState -> ParsingState
updateMidi name mid state = state {midi = updateList name mid (midi state)}

-- change the value of a music in the modified association list
updateModified :: Name -> Bool -> ParsingState -> ParsingState
updateModified name value state = state {modified = updateList name value (modified state)}

printValue :: ParsingState -> Name -> IO ()
printValue state name =
    let value = getValue state name
    in case value of
        Left  err       -> putStrLn err
        Right structure -> case structure of
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
transposeValue :: PSValue -> Int -> PSValue
transposeValue value num = 
    case value of 
        Left track  -> Left $ transposeT track num
        Right music -> Right $ transposeM music num 

-- flatten certain pitches in either a track or a music
flattenValue :: PSValue -> [Pitch] -> PSValue
flattenValue value ptchs =
    case value of 
        Left track  -> Left $ flattenT track ptchs
        Right music -> Right $ flattenM music ptchs 