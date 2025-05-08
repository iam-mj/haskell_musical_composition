module MIDI.ToMIDI where

import Codec.Midi
import Music.Data (Instrument, Music)
import MIDI.Performance
import MIDI.InstrChannel
import MIDI.Synthesizer

-- NOTE: RESEARCH 1 - find out more about division
-- NOTE: RESEARCH 2 - better notes on toDelta

-- TODO: TEST 1 - test that when having multiple tracks, one end of track does not affect the others

-- TODO: TASK 1 - can probably ignore rests to avoid them being introduced in octave 0

type MidiEvent       = (Ticks, Message)
type InstrumentTrack = (Instrument, [MusicEvent]) -- an instrument and the events which correspond to it

division = 96 :: Int -- FIXME: RESEARCH 1

saveMusic :: Music -> FilePath -> IO ()
saveMusic music file = exportFile file ((toMidi . perform) music)

playMusic :: Music -> IO ()
playMusic = playMidi . toMidi . perform

-- turns a performance into a midi value: Midi fileType timeDev tracks
-- Codec.Midi's fromAbsTime changes the timestamps from absolute to relative toDelta times
toMidi :: Performance -> Midi
toMidi performance = 
    let pairs = splitByInstr performance
        icmap = makeICMap (map fst pairs)
    in Midi (if length pairs == 1 
                then SingleTrack 
                else MultiTrack)
            (TicksPerBeat division)
            (map (addEnd . fromAbsTime . makeTrack icmap) pairs)

-- -- splits a performance into an association list of instruments and the events which are played by them
-- -- need it to be able to separeate a performance into midi tracks
-- splitByInst :: Performance -> [InstrumentTrack]
-- splitByInst []   = []
-- splitByInst perf = split perf []
--     where split [] pairs               = pairs 
--           split (event : events) pairs = let inst       = eInst event
--                                              instEvents = lookup inst pairs
--                                          in case instEvents of
--                                             Nothing      -> split events ((inst, [event]) : pairs)
--                                             Just iEvents -> 
--                                                 let newEvents = insertEvent event iEvents
--                                                 in split events (addAssociation inst newEvents pairs)

-- -- adds a new event to an instrument key in an association list
-- addAssociation :: Instrument -> [MusicEvent] -> [InstrumentTrack] -> [InstrumentTrack]
-- addAssociation key events list = newAssoc key events list []
--     where newAssoc inst events [] newList = newList
--           newAssoc inst events (current@(cInst, cEvents) : pairs) newList
--             | cInst == inst = newList ++ (cInst, events) : pairs
--             | otherwise     = newAssoc inst events pairs (current : newList)

-- ticks after the last event after which comes the end of track
-- in order to prevent unwanted clipping
waitTillEnd = 256 :: Ticks

-- FIXME: TEST 1
-- add the end of track event
addEnd :: [MidiEvent] -> [MidiEvent]
addEnd msgList = msgList ++ [(waitTillEnd, TrackEnd)]

-- we'll use the NoteOn, NoteOff, ProgramChange, TempoChange
-- make a track for each instrument
makeTrack :: InstrumentChannelMap -> InstrumentTrack -> [MidiEvent]
makeTrack icmap (inst, events) = 
    let (ch, prgNum) = lookupChannel icmap inst
        instrEvent   = (0, ProgramChange ch prgNum)
        tempoEvent   = (0, TempoChange defTempo)
        melody []         = []
        melody (ev : evs) = let (start, stop) = makeMEvents ch ev
                            in start : insertMEvent stop (melody evs)
    in instrEvent : tempoEvent : melody events

-- each MusicEvent will transform into 2 Midi events: one NoteOn & one NoteOff
-- note: toDelta transforms relative beats (1 beat = 1 whole note) into ticks
makeMEvents :: Channel -> MusicEvent -> (MidiEvent, MidiEvent)
makeMEvents ch (MEvent {eTime = t, ePitch = pth, eDur = dur, eVol = v}) = 
    ((toDelta t, NoteOn ch pth (limit v)), (toDelta (t + dur), NoteOff ch pth (limit v)))
    where toDelta t = round (t * 2.0 * fromIntegral division) -- FIXME: RESEARCH 2
          limit v   = max 0 (min 127 v)

-- insert a midi event into a list of midi events so that the timestamps are in ascending order
insertMEvent :: MidiEvent -> [MidiEvent] -> [MidiEvent]
insertMEvent e [] = [e]
insertMEvent e@(t, _) (e1@(t1, _) : es) 
    | t < t1    = e  : e1 : es 
    | otherwise = e1 : insertMEvent e es
