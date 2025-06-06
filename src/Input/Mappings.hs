module Input.Mappings where

import Music.Data
import Text.Parsec.String
import Text.Parsec
import Data.Maybe
import MIDI.InstrChannel (gmsmap)

-- TODO: TASK 2 - add more instruments -> keep them closer to the original names?

stringToPitch :: [(String, Pitch)]
stringToPitch = [("Ab", Af), ("A#", As), ("A", A),
                 ("Bb", Bf), ("B#", Bs), ("B", B),
                 ("Cb", Cf), ("C#", Cs), ("C", C),
                 ("Db", Df), ("D#", Ds), ("D", D),
                 ("Eb", Ef), ("E#", Es), ("E", E),
                 ("Fb", Ff), ("F#", Fs), ("F", F),
                 ("Gb", Gf), ("G#", Gs), ("G", G)]

stringToDuration :: [(String, Duration)]
stringToDuration = [("bn", bn), ("wn", wn), ("hn", hn), ("qn", qn),
                    ("en", en), ("sn", sn), ("tn", tn), ("sfn", sfn)]

-- note: interval in semitones
stringToInterval :: [(String, Interval)]
stringToInterval = [("p1", 0),    ("min2", 1),  ("maj2", 2),  ("min3", 3), ("maj3", 4),
                    ("perf4", 5), ("tt", 6),    ("perf5", 7), ("min6", 8), ("maj6", 9),
                    ("min7", 10), ("maj7", 11), ("perf8", 12)]

stringToChord :: [(String, Chord)]
stringToChord = [("maj3", MajorTriad), ("min3", MinorTriad), ("dim3", DiminishedTriad), ("aug3", AugmentedTriad)]

stringToInstrument :: [(String, Instrument)]
stringToInstrument = [("violin", Violin), ("piano", AcousticGrandPiano), ("drums", MelodicDrum), ("flute", Flute)]

stringToGeneralMidi :: [(String, Instrument)]
stringToGeneralMidi = map (\(instrument, _) -> (show instrument, instrument)) gmsmap