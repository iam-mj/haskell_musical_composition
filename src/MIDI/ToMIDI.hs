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
            (map makeTrack pairs)

division = 96 :: Int -- TODO: find out more about this

-- splits a performance into an association list of instruments and the events which are played by them
-- TODO: test that the events are ordered by timestamp as much as possible !! will lose performance wise :(
--       but still probably necessary :(
splitByInst :: Performance -> [(Instrument, [MusicEvent])]
splitByInst [] = []
splitByInst perf = split perf []
    where split [] pairs               = pairs 
          split (event : events) pairs = let inst = eInst event
                                         in let instEvents = lookup inst pairs
                                            in case instEvents of
                                                Nothing -> split events ((inst, [event]) : pairs)
                                                Just iEvents -> split events (addAssociation inst event pairs)

-- adds a new event to an instrument key in an association list
addAssociation :: Instrument -> MusicEvent -> [(Instrument, [MusicEvent])] -> [(Instrument, [MusicEvent])]
addAssociation key event list = newAssoc key event list []
    where newAssoc inst event [] newList = newList
          newAssoc inst event (current@(key, events) : pairs) newList = 
            if key == inst then newAssoc inst event pairs ((key, event : events) : newList)
                           else newAssoc inst event pairs (current : newList)

-- we'll use the NoteOn, NoteOff, ProgramChange & TrackEnd
-- TODO: maybe use Tempo Change too?
makeTrack :: (Instrument, [MusicEvent]) -> [(Ticks, Message)]
makeTrack = undefined