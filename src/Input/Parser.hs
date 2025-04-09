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

-- end of line
eol :: Parser Char
eol = char '\n'

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

repLine :: Parser Int
repLine = undefined

rep :: Parser Integer
rep = undefined