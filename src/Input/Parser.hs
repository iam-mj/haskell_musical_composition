module Input.Parser where

import Input.Fundamental
import Input.Mappings
import Input.State
import Music.Data
import Music.Utils

import Text.Parsec
import Data.Maybe
import Control.Monad.IO.Class (liftIO)
import MIDI.ToMIDI

-- parse the music definition, then add it to the current state
musicParser :: MyParser ()
musicParser = do
    string "music"
    name <- identifier -- TODO: check that the name is unique
    track <- braces (eol >> musicDefinition)
    eol
    modifyState $ addTrack name track
    return ()

musicDefinition :: MyParser Track
musicDefinition = do
    groups <- many groups
    return $ link $ concat groups

groups :: MyParser [Group]
groups = noteLine <|> restLine <|> duoLine <|> chordLine

noteLine :: MyParser [Group]
noteLine = do
    string "note:"
    notes <- commaSep oneNote    -- TODO: test if it parses and just one note!
    rep   <- optionMaybe repLine -- TODO: do something with this!!
    eol
    return $ map Single notes

oneNote :: MyParser Primitive
oneNote = do
    (pitch, ch) <- pitchP
    dur         <- durP
    return (Note pitch dur ch)

pitchP :: MyParser (Pitch, OctaveChange)
pitchP = do
    pitch <- pitchClass
    ch    <- parens change
    return (pitch, fromIntegral ch)
    where change = (char '+' >> int) <|> int -- TODO: make sure it parses the "-" ok
          pitchClass = mapString stringToPitch

durP :: MyParser Duration
durP = do
    dur <- mapString stringToDuration
    rep <- optionMaybe rep -- TODO: do something with this! send it out of the parser
    return dur

restLine :: MyParser [Group]
restLine = do
    string "rest:"
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
    return $ map (uncurry Duo) duos

oneDuo :: MyParser (Interval, Primitive)
oneDuo = do
    int  <- int -- TODO: in my grammar example the interval was not an integer, make up your mind!
    note <- oneNote
    return (int, note)

chordLine :: MyParser [Group]
chordLine = do
    string "chord:"
    chords <- commaSep oneChord
    rep    <- optionMaybe repLine
    eol
    return $ map (uncurry Chord) chords

oneChord :: MyParser (Chord, Primitive)
oneChord = do
    chord <- mapString stringToChord
    note <- oneNote
    return (chord, note)

-- TODO: fix the '/' case + make sure it deals okay with these partial cases
repLine :: MyParser Int
repLine = (char '/' >> return 0) <|> parens rep <|> (char '/' >> parens rep)

rep :: MyParser Int
rep = char 'x' >> int

show :: MyParser ()
show = do
    string "show"
    spaces
    name  <- identifier
    state <- getState
    eol
    liftIO $ printValue state name
    return ()

-- TODO: check exception throws okay
context :: MyParser ()
context = do
    string "context"
    spaces
    name      <- identifier
    musicName <- identifier -- TODO: check that it's unique
    state     <- getState
    spaces
    oct <- int
    spaces
    instrument <- mapString stringToInstrument
    eol
    let Right track = getTrack state name
    let music = Music (interpret track) oct instrument
    modifyState $ addMusic musicName music
    return ()

play :: MyParser ()
play = do
    string "play"
    spaces
    name  <- identifier
    state <- getState
    eol
    let Right music = getMusic state name
    liftIO $ playMusic music
    return ()

save :: MyParser ()
save = do
    string "save"
    spaces
    name <- identifier
    spaces
    fileName <- many $ noneOf "\n "
    eol
    state <- getState
    let Right music = getMusic state name
    liftIO $ saveMusic music fileName -- TODO: maybe some kind of exception if file name is weird
    return ()

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
-- will have the show print the index of each group as well to make modifying easy
insert :: String -> MyParser ()
insert name = do
    string "insert"
    spaces
    idx <- index
    spaces
    insertName <- identifier
    state      <- getState
    let Right insert = getTrack state insertName
        Right track  = getTrack state name
        newTrack     = insertT track idx insert
    modifyState $ updateTrack name newTrack
    return ()

-- TODO: check it works on all indexes: first, last, middle, sequence
delete :: String -> MyParser ()
delete name = do
    string "delete"
    spaces
    idxs  <- indexes 
    state <- getState
    let Right track = getTrack state name
        newTrack    = deleteT track idxs
    modifyState $ updateTrack name newTrack
    return ()

replace :: String -> MyParser ()
replace name = do
    string "replace"
    spaces
    idxs <- indexes
    spaces
    replaceName <- identifier
    state       <- getState
    let Right track     = getTrack state name
        Right replaceTr = getTrack state replaceName
        newTrack        = replaceT track idxs replaceTr
    modifyState $ updateTrack name newTrack 
    return ()

index :: MyParser Int
index = int

-- TODO: make sure it parses it right
-- TODO: should check that the idexes are positive
indexes :: MyParser (Int, Maybe Int)
indexes = do
    int <- int
    return (int, Nothing)
    <|> do
    left <- int
    char '-'
    right <- int
    return (left, Just right)

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
    return ()

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
    return ()

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
            Left track  -> modifyState $ updateTrack name track
            Right music -> modifyState $ updateMusic name music 
    return ()
