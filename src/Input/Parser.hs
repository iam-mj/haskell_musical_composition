module Input.Parser where

import Input.Fundamental
import Input.Mappings
import Input.State
import Input.Helpers
import Music.Data hiding (track)

import Text.Parsec hiding (spaces)
import Prelude hiding (show)

-- TODO: TASK 13 - add a logs messages structure

mainParser :: MyParser ParsingState
mainParser = do
    choice $ map try [track, show, melody, save, play, modify]
    getState

-- parse the music definition, then add it to the current state
track :: MyParser ()
track = do
    string "track"
    spaces
    name <- identifier
    spaces
    track <- braces (eol >> define)
    eol
    tryAddTrack name track

define :: MyParser Track
define = do
    groups <- many (identation >> groups)
    groupsToTrack groups

groups :: MyParser [Group]
groups = choice $ map try [noteLine, restLine, duoLine, chordLine]

noteLine :: MyParser [Group]
noteLine = do
    string "note:"
    spaces
    notes <- commaSep oneNote
    rep   <- optionMaybe repLine
    eol
    notesToGroups notes rep

oneNote :: MyParser RepeatNote
oneNote = do
    spaces
    (pitch, ch) <- pitchP
    dur         <- durP
    rep         <- optionMaybe rep
    makeRepeatNote pitch dur ch rep

pitchP :: MyParser PitchWithChange
pitchP = do
    pitch <- pitchClass
    spaces
    ch    <- optionMaybe $ parens change
    makePitchWithChange pitch ch

change :: MyParser OctaveChange
change = (char '+' >> int) <|> int

pitchClass :: MyParser Pitch
pitchClass = mapString stringToPitch

durP :: MyParser Duration
durP = spaces >> mapString stringToDuration

restLine :: MyParser [Group]
restLine = do
    string "rest:"
    spaces
    dur <- durP
    rep <- optionMaybe repLine
    eol
    restToGroups dur rep

duoLine :: MyParser [Group]
duoLine = do
    string "duo:"
    duos <- commaSep oneDuo
    rep  <- optionMaybe repLine
    eol
    spaces
    duosToGroups duos rep

oneDuo :: MyParser RepeatDuo
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
    chordsToGroups chords rep

oneChord :: MyParser RepeatChord
oneChord = do
    spaces
    chord       <- mapString stringToChord
    (note, rep) <- oneNote
    return (chord, note, rep)

repLine :: MyParser Repeat
repLine = spaces >> parens rep

rep :: MyParser Repeat
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
    showAllHelper

showOne :: MyParser ()
showOne = do
    name  <- identifier
    eol
    showOneHelper name

melody :: MyParser ()
melody = do
    string "melody"
    spaces
    melodyName <- identifier
    spaces
    char '{'
    spaces
    name <- identifier
    spaces
    string "oct"
    oct <- int
    spaces
    instrument <- mapString stringToInstrument
    spaces
    char '}'
    eol
    tryAddMusic name melodyName oct instrument

play :: MyParser ()
play = do
    string "play"
    spaces
    playFile <|> playValue

playFile :: MyParser ()
playFile = do
    fileName <- quotes $ many $ noneOf "\n \""
    eol
    tryPlayFile fileName
    
playValue :: MyParser ()
playValue = do
    name  <- identifier
    eol
    tryPlayValue name

save :: MyParser ()
save = do
    string "save"
    spaces
    name <- identifier
    spaces
    fileName <- quotes $ many $ noneOf "\n \""
    eol
    saveToFile name fileName

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
modifyOp name = fmap (\parser -> parser name) [insert, delete, replace, parallelize, seque, trans, clean]

-- insert at a certain group index
-- print the index of each group at the show command as well to make modifying easy
insert :: Name -> MyParser ()
insert name = do
    string "insert"
    spaces
    index <- index
    spaces
    insertName <- identifier
    callInsert name index insertName

delete :: Name -> MyParser ()
delete name = do
    string "delete"
    spaces
    indexes <- indexes 
    callDelete name indexes

replace :: Name -> MyParser ()
replace name = do
    string "replace"
    spaces
    indexes <- indexes
    spaces
    replaceName <- identifier
    callReplace name indexes replaceName

index :: MyParser IndexOrError
index = do
    idx <- int
    makeIndex idx

indexes :: MyParser IndexesOrError
indexes = try (do
    left <- index
    char '-'
    right <- index
    makeIndexes2 left right
    ) <|> try (do
    idx <- index
    makeIndexes1 idx
    )

-- only for Music values
parallelize :: Name -> MyParser ()
parallelize name = do
    string "||"
    spaces
    paraName <- identifier
    callParallelize name paraName

-- very awkward name but others clashed with Prelude functions
-- only for tracks
seque :: Name -> MyParser ()
seque name = do
    string "++"
    num <- optionMaybe int
    spaces
    seqName <- identifier
    callSequence name num seqName

-- for both tracks and music
trans :: Name -> MyParser ()
trans name = do
    string "transpose"
    spaces
    num   <- int
    callTranspose name num

clean :: Name -> MyParser ()
clean name = do
    string "clean"
    callClean name