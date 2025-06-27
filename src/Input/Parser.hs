module Input.Parser(runParser) where

import Input.Fundamental
import Input.Mappings
import Input.State
import Input.Helpers
import Music.Data
    ( Track, Group, Pitch, Duration, Chord, OctaveChange, Instrument )

import Text.Parsec hiding (spaces, parse, runParser)
import Prelude hiding (show, read)
import Control.Monad.Cont (liftIO)
import Input.Help
import Text.Parsec (space)
import System.IO
import System.Exit

-- TODO: TASK 18 - any chance not to show "pm_winmm_term called/exting" messages? nope :,)

-- NOTE: we accept both mappings and ints for interval values


-------------------------------------------
--               PARSING                 --
-------------------------------------------

-------------- UTILS -----------------

-- check if a bracket was closed in the line (only in "track" instr)
closeBracket :: String -> Bool
closeBracket line = notElem '{' line && '}' `elem` line

-- check if a bracket was opened and left like that (only in "track" instr)
openBracket :: String -> Bool
openBracket line = '{' `elem` line && notElem '}' line

-- we want to ignore blank lines
isSpaces :: String -> Bool
isSpaces = foldl (\bool chr -> bool && (chr `elem` " \n\t")) True


--------------- IO -------------------

runParser :: IO ()
runParser = putStrLn "\n|: PRELUDE :|\n" >> parse "" emptyState False

-- note: we keep on parsing lines only if an open bracket was not closed
parse :: String -> ParsingState -> Bool -> IO ()
parse buffer state bracket = do
    putStr "prelude> "
    hFlush stdout
    line   <- getLine
    if line == "exit"
        then exitSuccess
        else if isSpaces line
                then parse buffer state bracket
                else do
                    let newBuffer    = buffer ++ line ++ "\n"
                        stillOpen    = not $ closeBracket line
                        leaveBracket = openBracket line
                    if (bracket && stillOpen) || leaveBracket
                        then parse newBuffer state True
                        else do
                            result <- runParserT mainParser state "" newBuffer
                            case result of
                                Left err       -> print err >> parse "" state False
                                Right newState -> parse "" newState False

---------------- FILES -------------------

type ParseArgs = (String, ParsingState, Bool)

parseFile :: FilePath -> MyParser ()
parseFile file = do
    state  <- getState
    lines  <- getFileLines file
    let initialArgs = liftIO $ return $ Right ("", state, False)
    readResult <- liftIO $ foldl parseLines initialArgs lines
    manageReadResult file readResult

parseLines :: IO (Either String ParseArgs) -> String -> IO (Either String ParseArgs)
parseLines args line = do
    lastResult <- args
    case lastResult of
        Left err                       -> return $ Left err
        Right (buffer, state, bracket) -> do
            if isSpaces line
                then return $ Right (buffer, state, bracket)
                else do
                    let newBuffer    = buffer ++ line ++ "\n"
                        stillOpen    = not $ closeBracket line
                        leaveBracket = openBracket line
                    if (bracket && stillOpen) || leaveBracket
                        then return $ Right (newBuffer, state, True)
                        else do
                            result <- runParserT mainParser state "" newBuffer
                            case result of
                                Left err       -> return $ Left $ showErr err
                                Right newState -> return $ Right ("", newState, False)


mainParser :: MyParser ParsingState
mainParser = do
    choice $ map try [track, melody, show, play, save, load, read, score, vis, see, modify, help]
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
durP = do
    spaces
    dur <- mapString stringToDuration
    dot <- optionMaybe $ char '.'
    makeDuration dur dot

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
    int         <- mapString stringToInterval <|> nat
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
    chord       <- mapString stringToChord <|> customChord
    (note, rep) <- oneNote
    return (chord, note, rep)

customChord :: MyParser Chord
customChord = do
    char '<'
    spaces
    char '0'
    spaces
    char ','
    digits <- commaSep $ spaces >> many digit
    char '>'
    makeCustomChord digits

---------------- MELODY ---------------------

melody :: MyParser ()
melody = do
    string "melody"
    spaces
    melodyName <- identifier
    spaces
    (name, oct, instrument) <- context
    eol
    tryAddMusic name melodyName oct instrument

context :: MyParser (Name, Int, Instrument)
context = do
    char '{'
    spaces
    name <- identifier
    spaces
    string "oct"
    oct <- int
    spaces
    instrument <- mapString stringToInstrument <|> mapString stringToGeneralMidi
    spaces
    char '}'
    return (name, oct, instrument)


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
    playTrack <|> playFile <|> playValue

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

playTrack :: MyParser ()
playTrack = do
    (name, oct, instrument) <- context
    eol
    tryPlayTrack name oct instrument

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

---------------- READ ----------------------

read :: MyParser ()
read = do
    string "read"
    spaces
    file <- fileName
    eol
    parseFile file

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
    htmlMidiFromFile file

scoreMusic :: MyParser ()
scoreMusic = do
    music <- identifier
    eol
    htmlMidiFromMusic music

---------------- VIS ----------------------

vis :: MyParser ()
vis = do
    string "vis"
    spaces
    visFile <|> visMusic

visFile :: MyParser ()
visFile = do
    file <- fileName
    eol
    visFromFile file

visMusic :: MyParser ()
visMusic = do
    music <- identifier
    eol
    visFromMusic music

----------------- SEE ---------------------

see :: MyParser ()
see = do
    string "see"
    spaces
    (name, oct, instr) <- context
    eol
    seeVis name oct instr

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
modifyOp name = fmap (\parser -> parser name) [insert, delete, replace, parallelize, seque, trans, flatten]

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
    num <- optionMaybe nat
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

---------------- FLATTEN -------------------

-- for both tracks and music
flatten :: Name -> MyParser ()
flatten name = do
    string "flatten"
    pitches <- commaSep (spaces >> pitchClass)
    callFlatten name pitches

--------------- HELP -----------------------

help :: MyParser ()
help = do
    string "help"
    spaces
    generalHelp <|> instructionHelp

-------------------------------------------
--             HELPERS                   -- 
-------------------------------------------


repLine :: MyParser Repeat
repLine = parens rep

rep :: MyParser Repeat
rep = char 'x' >> nat

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
    ) <|> do
    idx <- index
    makeIndexes1 idx

fileName :: MyParser String
fileName = quotes $ many $ noneOf " \""