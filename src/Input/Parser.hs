module Input.Parser where

import Input.Fundamental
import Input.Mappings
import Input.State
import Input.Helpers
import Music.Data hiding (track)

import Text.Parsec hiding (spaces)
import Prelude hiding (show)


-- TODO: TASK 14 - in my grammar example the interval was not an integer, make up your mind
-- TODO: TASK 15 - allow spaces at the end of a line
-- TODO: TASK 16 - if file doesn't exist, please don't stop the whole app!


mainParser :: MyParser ParsingState
mainParser = do
    choice $ map try [track, melody, show, save, load, play, score, modify]
    getState


-------------------------------------------
--             DEFINITION                --
-------------------------------------------


---------------- TRACK ---------------------

-- parse the track definition, then add it to the current state
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

---------------- NOTE ---------------------

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
    spaces
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

---------------- REST ---------------------

restLine :: MyParser [Group]
restLine = do
    string "rest:"
    spaces
    dur <- durP
    rep <- optionMaybe repLine
    eol
    restToGroups dur rep

---------------- DUO ---------------------

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
    int         <- int       -- FIXME: TASK 14
    (note, rep) <- oneNote
    return (int, note, rep)

---------------- CHORD ---------------------

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

---------------- MELODY ---------------------

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


-------------------------------------------
--             SYSTEM OPS                -- 
-------------------------------------------


---------------- SHOW ---------------------

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

---------------- PLAY ---------------------

play :: MyParser ()
play = do
    string "play"
    spaces
    playFile <|> playValue

playFile :: MyParser ()
playFile = do
    file <- fileName
    eol
    tryPlayFile file
    
playValue :: MyParser ()
playValue = do
    name  <- identifier
    eol
    tryPlayValue name

---------------- SAVE ---------------------

save :: MyParser ()
save = do
    string "save"
    spaces
    name <- identifier
    spaces
    file <- fileName
    eol
    saveToFile name file

---------------- LOAD ----------------------

load :: MyParser ()
load = do
    string "load"
    spaces
    file <- fileName
    spaces
    name <- identifier
    eol
    loadFromFile name file

--------------- SCORE ----------------------

score :: MyParser ()
score = do
    string "score"
    spaces
    scoreFile <|> scoreMusic

scoreFile :: MyParser ()
scoreFile = do
    file <- fileName
    eol
    createScoreFromFile file

scoreMusic :: MyParser ()
scoreMusic = do
    music <- identifier
    eol
    createScoreFromMusic music

--------------- MODIFY ---------------------

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

--------------- INSERT ---------------------

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

--------------- DELETE ---------------------

delete :: Name -> MyParser ()
delete name = do
    string "delete"
    spaces
    indexes <- indexes 
    callDelete name indexes

--------------- REPLACE ---------------------

replace :: Name -> MyParser ()
replace name = do
    string "replace"
    spaces
    indexes <- indexes
    spaces
    replaceName <- identifier
    callReplace name indexes replaceName

------------- PARALLELIZE ------------------

-- only for Music values
parallelize :: Name -> MyParser ()
parallelize name = do
    string "||"
    spaces
    paraName <- identifier
    callParallelize name paraName

-------------- SEQUENCE --------------------

-- very awkward name but others clashed with Prelude functions
-- only for tracks
seque :: Name -> MyParser ()
seque name = do
    string "++"
    num <- optionMaybe int
    spaces
    seqName <- identifier
    callSequence name num seqName

-------------- TRANSPOSE -------------------

-- for both tracks and music
trans :: Name -> MyParser ()
trans name = do
    string "transpose"
    spaces
    num   <- int
    callTranspose name num

--------------- CLEAN ---------------------

clean :: Name -> MyParser ()
clean name = do
    string "clean"
    callClean name


-------------------------------------------
--             HELPERS                   -- 
-------------------------------------------


repLine :: MyParser Repeat
repLine = parens rep

rep :: MyParser Repeat
rep = char 'x' >> int

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

fileName :: MyParser String
fileName = quotes $ many $ noneOf " \""