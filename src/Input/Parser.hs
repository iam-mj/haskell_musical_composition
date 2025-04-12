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
    name <- identifier
    -- TODO: get the music
    spaces
    modifyOp 
    eol
    return ()

modifyOp :: MyParser ()
modifyOp = insert <|> delete <|> replace <|> parallelize <|> seque <|> trans

insert :: MyParser ()
insert = do
    string "insert"
    spaces
    id <- index
    spaces
    insertValue <- identifier
    -- TODO: check and get insert value + also actually do the insert
    return ()

delete :: MyParser ()
delete = do
    string "delete"
    spaces
    idxs <- indexes 
    -- TODO: delete the indexes
    return ()

replace :: MyParser ()
replace = do
    string "replace"
    spaces
    idxs <- indexes
    spaces
    replaceValue <- identifier
    -- TODO: replace the indexes
    return ()

index :: MyParser Int
index = int

-- TODO: make sure it parses it right
indexes :: MyParser [Int]
indexes = do
    int <- int
    return [int] 
    <|> do
    left <- int
    char '-'
    right <- int
    return [left, right]

parallelize :: MyParser ()
parallelize = do
    string "||"
    spaces
    name <- identifier
    -- TODO: get it and parallelize it
    return ()

-- very awkward name but others clashed with Prelude functions
seque :: MyParser ()
seque = do
    string "++"
    num <- optionMaybe int -- TODO: handle repeated sequencing
    spaces
    name <- identifier
    -- TODO: actually sequence
    return ()

trans :: MyParser ()
trans = do
    string "transpose"
    spaces
    num <- int
    -- TODO: actually transpose with that number or semi-tones
    return ()
