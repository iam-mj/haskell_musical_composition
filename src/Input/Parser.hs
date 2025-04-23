module Input.Parser where

import Input.Fundamental
import Input.Mappings
import Input.State
import MIDI.ToMIDI
import Music.Data
import Music.Utils

import Text.Parsec
import Data.Maybe
import Control.Monad.IO.Class (liftIO)
import Text.Parsec.Token (comma)
import Prelude hiding (show)

-- TODO: TASK 2  - repLine functionality
-- TODO: TASK 3  - repLine parser okay?
-- TODO: TASK 5  - separate functionality from parser as much as possible
-- TODO: TASK 7  - aliases for weird types
-- TODO: TASK 8  - play file
-- TODO: TASK 10 - might have to rename the "music" command as "track" + context

mainParser :: MyParser ParsingState
mainParser = do
    choice $ map try [musicParser, show, context, save, play, modify]
    getState

-- parse the music definition, then add it to the current state
musicParser :: MyParser ()
musicParser = do
    string "music"
    spaces
    name <- identifier
    spaces
    track <- braces (eol >> musicDefinition)
    eol
    state    <- getState
    newState <- liftIO $ addTrack name track state
    case newState of
        Nothing    -> do
            putState state
            liftIO $ putStrLn $ "Failed to add track " ++ name
        Just newSt -> do
            putState newSt
            liftIO $ putStrLn $ "Track " ++ name ++ " added succesfully"

musicDefinition :: MyParser Track
musicDefinition = do
    groups <- many (identation >> groups)
    return $ link $ concat groups

groups :: MyParser [Group]
groups = choice $ map try [noteLine, restLine, duoLine, chordLine]

noteLine :: MyParser [Group]
noteLine = do
    string "note:"
    spaces
    notes <- commaSep oneNote
    rep   <- optionMaybe repLine -- FIXME: TASK 2
    eol
    spaces
    return $ map Single notes

oneNote :: MyParser Primitive
oneNote = do
    spaces
    (pitch, ch) <- pitchP
    dur         <- durP
    return (Note pitch dur ch)

noChange = 0 :: OctaveChange

pitchP :: MyParser (Pitch, OctaveChange)
pitchP = do
    pitch <- pitchClass
    spaces
    ch    <- optionMaybe $ parens change
    case ch of
        Nothing -> return (pitch, noChange)
        Just ch -> return (pitch, fromIntegral ch)
    where change = (char '+' >> int) <|> int
          pitchClass = mapString stringToPitch

durP :: MyParser Duration
durP = do
    spaces
    dur <- mapString stringToDuration
    rep <- optionMaybe rep -- FIXME: TASK 2
    return dur

restLine :: MyParser [Group]
restLine = do
    string "rest:"
    spaces
    rest <- durP
    rep  <- optionMaybe repLine
    eol
    return [(Single . Rest) rest]

duoLine :: MyParser [Group]
duoLine = do
    string "duo:"
    duos <- commaSep oneDuo
    rep  <- optionMaybe repLine
    eol
    spaces
    return $ map (uncurry Duo) duos

oneDuo :: MyParser (Interval, Primitive)
oneDuo = do
    spaces
    int  <- int -- TODO: in my grammar example the interval was not an integer, make up your mind!
    note <- oneNote
    return (int, note)

chordLine :: MyParser [Group]
chordLine = do
    string "chord:"
    chords <- commaSep oneChord
    rep    <- optionMaybe repLine
    eol
    spaces
    return $ map (uncurry Chord) chords

oneChord :: MyParser (Chord, Primitive)
oneChord = do
    spaces
    chord <- mapString stringToChord
    note  <- oneNote
    return (chord, note)

-- FIXME: TASK 3
repLine :: MyParser Int
repLine = (char '/' >> return 0) <|> parens rep <|> (char '/' >> parens rep)

rep :: MyParser Int
rep = char 'x' >> int

show :: MyParser ()
show = do
    string "show"
    spaces
    showAll <|> showOne

showAll :: MyParser ()
showAll = do
    string "-all"
    eol
    state <- getState
    liftIO $ printState state

showOne :: MyParser ()
showOne = do
    name  <- identifier
    state <- getState
    eol
    liftIO $ printValue state name

context :: MyParser ()
context = do
    string "context"
    spaces
    name      <- identifier
    spaces
    musicName <- identifier
    state     <- getState
    spaces
    oct <- int
    spaces
    instrument <- mapString stringToInstrument
    eol
    let Right track = getTrack state name
    let music = Music (interpret track) oct instrument
    newState <- liftIO $ addMusic musicName music state
    case newState of
        Nothing    -> do
            putState state
            liftIO $ putStrLn $ "Failed to add melody " ++ musicName
        Just newSt -> do
            putState newSt
            liftIO $ putStrLn $ "Melody " ++ musicName ++ " added succesfully"

play :: MyParser ()
play = do
    string "play"
    spaces
    name  <- identifier
    state <- getState
    eol
    let Right music = getMusic state name
    liftIO $ playMusic music

save :: MyParser ()
save = do
    string "save"
    spaces
    name <- identifier
    spaces
    fileName <- quotes $ many $ noneOf "\n \""
    eol
    let validationErr = validatePath fileName
    case validationErr of
        Nothing -> do
            state <- getState
            let Right music = getMusic state name
            liftIO $ saveMusic music fileName
            liftIO $ putStrLn $ "File " ++ fileName ++ " saved succesfully"
        Just err -> liftIO $ print err

modify :: MyParser ()
modify = do
    string "modify"
    spaces
    name  <- identifier
    spaces
    choice $ modifyOp name 
    eol
    return ()

modifyOp :: String -> [MyParser ()]
modifyOp name = fmap (\parser -> parser name) [insert, delete, replace, parallelize, seque, trans]

-- will insert at a certain group index
-- will print the index of each group at the show command as well to make modifying easy
insert :: String -> MyParser ()
insert name = do
    string "insert"
    spaces
    index <- index
    case index of
        Left err  -> liftIO $ putStrLn err
        Right idx -> do
            spaces
            insertName <- identifier
            state      <- getState
            let Right insert = getTrack state insertName
                Right track  = getTrack state name
                newTrack     = insertT track idx insert
            modifyState $ updateTrack name newTrack
            liftIO $ putStrLn $ "Track " ++ name ++ " modified succesfully"
            liftIO $ print newTrack

delete :: String -> MyParser ()
delete name = do
    string "delete"
    spaces
    indexes <- indexes 
    case indexes of
        Left err   -> liftIO $ putStrLn err
        Right idxs -> do
            state <- getState
            let Right track = getTrack state name
                newTrack    = deleteT track idxs
            modifyState $ updateTrack name newTrack
            liftIO $ putStrLn $ "Track " ++ name ++ " modified succesfully"
            liftIO $ print newTrack

replace :: String -> MyParser ()
replace name = do
    string "replace"
    spaces
    indexes <- indexes
    case indexes of
        Left err   -> liftIO $ putStrLn err
        Right idxs -> do
            spaces
            replaceName <- identifier
            state       <- getState
            let Right track     = getTrack state name
                Right replaceTr = getTrack state replaceName
                newTrack        = replaceT track idxs replaceTr
            modifyState $ updateTrack name newTrack
            liftIO $ putStrLn $ "Track " ++ name ++ " modified succesfully"
            liftIO $ print newTrack

index :: MyParser (Either String Int)
index = do
    idx <- int
    let err = checkIndex idx 
    case err of
        Nothing    -> return $ Right (idx - 1)
        Just error -> return $ Left error

indexes :: MyParser (Either String (Int, Maybe Int))
indexes = try (do
    left <- index
    case left of
        Left err      -> return $ Left err
        Right leftIdx -> do
            char '-'
            right <- index
            case right of
                Left err       -> return $ Left err
                Right rightIdx -> return $ Right (leftIdx, Just rightIdx)
    ) <|> try (do
    idx <- index
    case idx of
        Left err    -> return $ Left err
        Right index -> return $ Right (index, Nothing)
    )

-- only for Music values
parallelize :: String -> MyParser ()
parallelize name = do
    string "||"
    spaces
    paraName <- identifier
    state    <- getState
    let Right music     = getMusic state name
        Right paraMusic = getMusic state paraName
        newMusic        = music ::: paraMusic
    modifyState $ updateMusic name newMusic
    liftIO $ putStrLn $ "Melody " ++ name ++ " modified succesfully"
    liftIO $ print newMusic

-- very awkward name but others clashed with Prelude functions
-- only for tracks
seque :: String -> MyParser ()
seque name = do
    string "++"
    num <- optionMaybe int
    spaces
    seqName <- identifier
    state   <- getState
    let Right track = getTrack state name
        Right seqTr = getTrack state seqName
        newTrack    = addRepeatT track seqTr num
    modifyState $ updateTrack name newTrack
    liftIO $ putStrLn $ "Melody " ++ name ++ " modified succesfully"
    liftIO $ print newTrack

-- for both tracks and music
trans :: String -> MyParser ()
trans name = do
    string "transpose"
    spaces
    num   <- int
    state <- getState
    let value    = getValue state name
        newValue = transposeValue value num
    case newValue of
        Left _          -> return ()
        Right structure -> case structure of
            Left track  -> do
                modifyState $ updateTrack name track
                liftIO $ putStrLn $ "Track " ++ name ++ " was transposed succesfully"
                liftIO $ print track
            Right music -> do
                modifyState $ updateMusic name music 
                liftIO $ putStrLn $ "Melody " ++ name ++ " was transposed succesfully"
                liftIO $ print music

-- parse a string from an assciation list and return it's associated value
mapString :: [(String, a)] -> MyParser a
mapString list = do
    key <- choice (map ((try . string) . fst) list)
    return $ fromJust $ lookup key list