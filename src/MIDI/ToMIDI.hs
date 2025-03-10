module MIDI.ToMIDI where

import MIDI.Performance
import Codec.Midi
import Music.Data (Instrument)

-- can't make them directly synthetiser-midi values cause we might want to export them
-- turns a performance into a midi value: Midi fileType timeDev tracks
toMidi :: Performance -> Midi
toMidi performance = 
    let pairs = splitByInst performance
    in Midi (if length pairs == 1 
                then SingleTrack 
                else MultiTrack)
            (TicksPerBeat division)
            (makeTracks pairs)

division = 96 :: Int -- TODO: find out more about this

splitByInst :: Performance -> [(Instrument, [MusicEvent])]
splitByInst = undefined

makeTracks :: [(Instrument, [MusicEvent])] -> [[(Ticks, Message)]]
makeTracks = undefined