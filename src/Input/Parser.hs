module Input.Parser where

import Input.Fundamental
import Input.Mappings
import Input.State
import MIDI.ToMIDI
import Music.Data
import Music.Utils

import Text.Parsec hiding (spaces)
import Data.Maybe
import Control.Monad.IO.Class (liftIO)
import Text.Parsec.Token (comma)
import Prelude hiding (show)
import MIDI.Synthesizer (playMidiFile)

-- TODO: TASK 5  - separate functionality from parser as much as possible
-- TODO: TASK 10 - might have to rename the "music" command as "track" + context
-- TODO: TASK 11 - try to remove the IO () types in State, handle them outside thoese functions, just return the err
-- TODO: TASK 12 - add a "clean track" of empty tracks

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
    rep   <- optionMaybe repLine
    eol
    spaces
    let groups = map Single (concatMap (\(note, n) -> replicate n note) notes)
    case rep of
        Nothing     -> return groups
        Just repeat -> return $ repeatList repeat groups

oneNote :: MyParser (Primitive, Int)
oneNote = do
    spaces
    (pitch, ch) <- pitchP
    dur         <- durP
    rep         <- optionMaybe rep
    case rep of
        Nothing     -> return (Note pitch dur ch, 1)
        Just repeat -> return (Note pitch dur ch, repeat)
    

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
durP = spaces >> mapString stringToDuration

restLine :: MyParser [Group]
restLine = do
    string "rest:"
    spaces
    rest <- durP
    rep  <- optionMaybe repLine
    eol
    let groups = [(Single . Rest) rest]
    case rep of
        Nothing     -> return groups
        Just repeat -> return $ repeatList repeat groups

duoLine :: MyParser [Group]
duoLine = do
    string "duo:"
    duos <- commaSep oneDuo
    rep  <- optionMaybe repLine
    eol
    spaces
    let groups = map (uncurry Duo) (concatMap (\(int, note, rep) -> replicate rep (int, note)) duos)
    case rep of
        Nothing     -> return groups
        Just repeat -> return $ repeatList repeat groups

oneDuo :: MyParser (Interval, Primitive, Int)
oneDuo = do
    spaces
    int         <- int -- TODO: in my grammar example the interval was not an integer, make up your mind!
    (note, rep) <- oneNote
    return (int, note, rep)

chordLine :: MyParser [Group]
chordLine = do
    string "chord:"
    chords <- commaSep oneChord
    rep    <- optionMaybe repLine
    eol
    spaces
    let groups = map (uncurry Chord) (concatMap (\(chd, note, rep) -> replicate rep (chd, note)) chords)
    case rep of
        Nothing     -> return groups
        Just repeat -> return $ repeatList repeat groups

oneChord :: MyParser (Chord, Primitive, Int)
oneChord = do
    spaces
    chord       <- mapString stringToChord
    (note, rep) <- oneNote
    return (chord, note, rep)

repLine :: MyParser Int
repLine = spaces >> parens rep

rep :: MyParser Int
rep = spaces >> char 'x' >> int

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
    string "oct"
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
    playFile <|> playValue

playFile :: MyParser ()
playFile = do
    fileName <- quotes $ many $ noneOf "\n \""
    eol
    let validationErr = validatePath fileName
    case validationErr of
        Nothing  -> liftIO $ playMidiFile fileName
        Just err -> liftIO $ print err
    
playValue :: MyParser ()
playValue = do
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

modifyOp :: Name -> [MyParser ()]
modifyOp name = fmap (\parser -> parser name) [insert, delete, replace, parallelize, seque, trans]

-- will insert at a certain group index
-- will print the index of each group at the show command as well to make modifying easy
insert :: Name -> MyParser ()
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

delete :: Name -> MyParser ()
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

replace :: Name -> MyParser ()
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

type Index   = Int
type Indexes = (Index, Maybe Index) -- one index (e.g. 1) or the limits of an interval of indexes (e.g. 2-4) 

index :: MyParser (Either Error Index)
index = do
    idx <- int
    let err = checkIndex idx 
    case err of
        Nothing    -> return $ Right (idx - 1)
        Just error -> return $ Left error

indexes :: MyParser (Either Error Indexes)
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
parallelize :: Name -> MyParser ()
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
seque :: Name -> MyParser ()
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
trans :: Name -> MyParser ()
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