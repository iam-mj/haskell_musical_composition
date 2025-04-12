module Input.Mappings where

import Music.Data
import Text.Parsec.String
import Text.Parsec
import Data.Maybe
import Input.State

-- parse a string from an assciation list and return it's associated value
mapString :: [(String, a)] -> MyParser a
mapString list = do
    key <- choice (map ((try . string) . fst) list)
    return $ fromJust $ lookup key list

stringToPitch :: [(String, Pitch)]
stringToPitch = [("A", A), ("Ab", Af), ("A#", As),
                 ("B", B), ("Bb", Bf), ("B#", Bs),
                 ("C", C), ("Cb", Cf), ("C#", Cs),
                 ("D", D), ("Db", Df), ("D#", Ds),
                 ("E", E), ("Eb", Ef), ("E#", Es),
                 ("F", F), ("Fb", Ff), ("F#", Fs),
                 ("G", G), ("Gb", Gf), ("G#", Gs)]

stringToDuration :: [(String, Duration)]
stringToDuration = [("bn", bn), ("wn", wn), ("hn", hn), ("qn", qn),
                    ("en", en), ("sn", sn), ("tn", tn), ("sfn", sfn)]

-- TODO: i have very few chords... also what about the custom chords?
stringToChord :: [(String, Chord)]
stringToChord = [("maj3", MajorThird), ("min3", MinorThird)]

-- TODO: add some more - maybe keep the names closer to the originals
stringToInstrument :: [(String, Instrument)]
stringToInstrument = [("violin", Violin), ("piano", AcousticGrandPiano)]