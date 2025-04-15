module Input.Mappings where

import Music.Data
import Text.Parsec.String
import Text.Parsec
import Data.Maybe

-- TODO: ok to put this here, or should make a different file?
errorMessages :: [(String, String)]
errorMessages = map (\(key, val) -> (key, "Error: " ++ val)) [
    ("NotUniqueName", "There is already a value recorded with the given name. Please enter a unique name."),
    ("NoTrackNameFound", "No tracks found with the given name!"),
    ("NoMelodyNameFound", "No melodies found with the given name!"),
    ("NoNameFound", "No structures found with the provided name")]

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