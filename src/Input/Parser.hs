module Input.Parser where

import Input.Lexer
import Input.Mappings
import Music.Data
import Music.Utils
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Text.Parsec.Token as Lex
import Control.Exception (NonTermination)
import Data.Maybe
import GHC.IORef (atomicModifyIORefP)

-- end of line
eol :: Parser Char
eol = char '\n'

-- TODO: gotta store this also!!
musicParser :: Parser TrackE
musicParser = do
    myReserved "music"
    musicName <- myIdentifier
    myBraces (eol >> musicDefinition)

musicDefinition :: Parser TrackE
musicDefinition = do
    groups <- many groups
    return $ interpret $ link $ concat groups

-- TODO: maybe Line names instead?
groups :: Parser [Group]
groups = noteP <|> restP <|> duoP <|> chordP

noteP :: Parser [Group]
noteP = do
    myReserved "note:"
    notes <- myCommaSep1 oneNote
    rep   <- optionMaybe repLine -- TODO: do something with this!!
    eol
    return $ map Single notes

oneNote :: Parser Primitive
oneNote = do
    (pitch, ch) <- pitchP
    dur <- durP
    return (Note pitch dur ch)

pitchP :: Parser (Pitch, OctaveChange)
pitchP = do
    pitch <- pitchClass
    ch <- myParens change
    return (pitch, fromIntegral ch)
    where change = (char '+' >> myInteger) <|> myInteger -- TODO: make sure it parses the "-" ok
          pitchClass = mapString stringToPitch

-- parse a string from an assciation list and return it's associated value
mapString :: [(String, a)] -> Parser a
mapString list = do
    key <- choice (map ((try . string) . fst) list)
    return $ fromJust $ lookup key list

durP :: Parser Duration
durP = do
    dur <- mapString stringToDuration
    rep <- optionMaybe rep -- TODO: do something with this! send it out of the parser
    return dur

restP :: Parser [Group]
restP = do
    myReserved "rest:"
    rest <- durP
    rep  <- optionMaybe repLine
    eol
    return [(Single . Rest) rest]

duoP :: Parser [Group]
duoP = do
    myReserved "duo:"
    duos <- myCommaSep1 oneDuo
    rep  <- optionMaybe repLine
    eol
    return $ map (uncurry Duo) duos

oneDuo :: Parser (Interval, Primitive)
oneDuo = do
    int  <- myInteger -- TODO: in my grammar example the interval was not an integer, make up your mind!
    note <- oneNote
    return (fromInteger int, note)

chordP :: Parser [Group]
chordP = do
    myReserved "chord:"
    chords <- myCommaSep1 oneChord
    rep    <- optionMaybe repLine
    eol
    return $ map (uncurry Chord) chords

oneChord :: Parser (Chord, Primitive)
oneChord = do
    chord <- mapString stringToChord
    note <- oneNote
    return (chord, note)

-- TODO: fix the '/' case + make sure it deals okay with these partial cases
repLine :: Parser Integer
repLine = (char '/' >> return 0) <|> myParens rep <|> (char '/' >> myParens rep)

rep :: Parser Integer
rep = do
    char 'x'
    myInteger

show :: Parser ()
show = do
    myReserved "show"
    char ' '
    name <- myIdentifier -- TODO: gotta actually print this music variable!!
    eol
    return ()

-- TODO: all the char ' ' are necessary?
context :: Parser Music
context = do
    myReserved "context"
    char ' '
    name <- myIdentifier
    let tracks = getMusic name -- TODO: handle exception!
    char ' '
    oct <- myInteger
    char ' '
    instrument <- mapString stringToInstrument
    eol
    return $ Music tracks (fromInteger oct) instrument --TODO: actually save this!!

-- TODO: will have to pass the variables dictionary
getMusic :: String -> TrackE
getMusic = undefined

play :: Parser ()
play = do
    myReserved "play"
    char ' '
    name <- myIdentifier
    -- TODO: actually get it, check it's music and play it
    eol
    return ()

save :: Parser ()
save = do
    myReserved "save"
    char ' '
    name <- myIdentifier
    -- TODO: get it & check it's music
    char ' '
    fileName <- noneOf "\n "
    eol
    -- TODO: try to save or print error
    return ()

modify :: Parser ()
modify = do
    myReserved "modify"
    char ' '
    name <- myIdentifier
    -- TODO: get the music
    char ' '
    modifyOp 
    eol
    return ()

modifyOp :: Parser ()
modifyOp = insert <|> delete <|> replace <|> parallelize <|> seque <|> trans

insert :: Parser ()
insert = do
    myReserved "insert"
    char ' '
    id <- index
    char ' '
    insertValue <- myIdentifier
    -- TODO: check and get insert value + also actually do the insert
    return ()

delete :: Parser ()
delete = do
    myReserved "delete"
    char ' '
    idxs <- indexes 
    -- TODO: delete the indexes
    return ()

replace :: Parser ()
replace = do
    myReserved "replace"
    char ' '
    idxs <- indexes
    char ' '
    replaceValue <- myIdentifier
    -- TODO: replace the indexes
    return ()

index :: Parser Integer
index = myInteger

-- TODO: make sure it parses it right
indexes :: Parser [Integer]
indexes = do
    int <- myInteger
    return [int] 
    <|> do
    left <- myInteger
    char '-'
    right <- myInteger
    return [left, right]

parallelize :: Parser ()
parallelize = do
    myReservedOp "||"
    char ' '
    name <- myIdentifier
    -- TODO: get it and parallelize it
    return ()

-- very awkward name but others clashed with Prelude functions
seque :: Parser ()
seque = do
    myReservedOp "++"
    num <- optionMaybe myInteger -- TODO: handle repeated sequencing
    char ' '
    name <- myIdentifier
    -- TODO: actually sequence
    return ()

trans :: Parser ()
trans = do
    myReserved "transpose"
    char ' '
    num <- myInteger
    -- TODO: actually transpose with that number or semi-tones
    return ()
