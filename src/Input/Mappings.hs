module Input.Mappings where

import Music.Data
import Text.Parsec.String
import Text.Parsec
import Data.Maybe

-- TODO: TASK 1 - add more chords -> what about custom?
-- TODO: TASK 2 - add more instruments -> keep them closer to the original names?

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

stringToChord :: [(String, Chord)]
stringToChord = [("maj3", MajorThird), ("min3", MinorThird)]

stringToInstrument :: [(String, Instrument)]
stringToInstrument = [("violin", Violin), ("piano", AcousticGrandPiano)]