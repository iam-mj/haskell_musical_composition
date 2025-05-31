module Main where

import Music.Data
import Music.Utils
import MIDI.Performance
import Input.Parser (mainParser)
import Input.State

import Text.Parsec hiding (parse)
import System.IO
import Data.Either (fromRight)

-- TODO: TASK 1 - put parse in library

main :: IO ()
main = parse "" emptyState

parse :: String -> ParsingState -> IO ()
parse buffer state = do
    putStr "musically> "
    hFlush stdout
    line   <- getLine
    if line /= "" then do
        let newBuffer = buffer ++ line ++ "\n"
        parse newBuffer state
    else do
        result <- runParserT mainParser state "<stdin>" buffer
        case result of
            Left err       -> print err >> parse "" state
            Right newState -> parse "" newState


-- test values
myGroup = single $ noteDef A 0.5
myDuo = fromRight $ duo 2 (noteDef C 1)
myChord = fromRight $ chord MinorTriad (noteDef D 0.5)
myTrack = track myGroup

-- uncleanTrack = linkT [EmptyT, EmptyT, EmptyT, myTrack, EmptyT, EmptyT, myTrack, EmptyT]
-- cleanTrack = cleanT uncleanTrack

-- trackE = interpret $ link [myGroup, myDuo, myChord]

-- m1 = music [trackE] 4 Violin
-- m2 = music [trackE, trackE] 4 Violin
-- m3 = music [trackE, trackE, trackE] 4 Violin

-- p1 = perform m1
-- p2 = perform m2

-- -- let's try composing Merry-Go-Round of Life
-- q1 = repeatT 3 $ link [duo 3 (note B en), duo 4 (noteInc C en)]
-- q2 = repeatT 3 $ link [duo 3 (note A en), duo 3 (note B en)]
-- q3 = repeatT 3 $ link [duo 4 (note G en), duo 3 (note A en)]
-- tr1 = linkT [q1, q2, q3]

-- mgr = music [interpret tr1] 4 AcousticGrandPiano

-- -- ex1

-- singleA = single $ note A en
-- singleB = single $ note B hn
-- singleR = single $ rest en
-- duoB    = duo 2 $ note B hn

-- track1 = interpret $ link [singleA, singleB, singleR, duoB]

-- melody = music [track1] 4 Violin ::: music [track1] 5 AcousticGrandPiano