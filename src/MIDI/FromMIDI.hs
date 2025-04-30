module MIDI.FromMIDI where

import Codec.Midi
import Music.Data (Instrument, Music, Duration)
import MIDI.Performance
import MIDI.InstrChannel
import MIDI.Synthesizer
import MIDI.ToMIDI (MidiEvent, division)
import Data.Maybe (fromJust)

-- TODO: TEST 1 - make sure the first events really are the instrument + tempo sets

-- transform a midi value into a performance
fromMidi :: Midi -> Performance
fromMidi midi = concatMap trackToEvents (tracks midi)

-- transform a track to its corresponding music events
-- FIXME: TEST 1
trackToEvents :: [MidiEvent] -> Performance
trackToEvents (instrEvent : tempoEvent : events) = 
    let instr = getInstrument instrEvent
    in fromRelativeTime $ midiToMusicEvent instr events

getInstrument :: MidiEvent -> Instrument
getInstrument (_, ProgramChange _ pgrNum) = fromJust $ lookup pgrNum reverseGmsmap

midiToMusicEvent :: Instrument -> [MidiEvent] -> [MusicEvent]
midiToMusicEvent instr [(_, TrackEnd)]          = []
midiToMusicEvent instr ((_, NoteOff {}) : rest) = midiToMusicEvent instr rest
midiToMusicEvent instr ((ticks, NoteOn _ ptch _) : rest) =
    let time                 = ticksToTime ticks
        (offEvent, offTicks) = lookAhead ptch rest
        offTime              = ticksToTime offTicks
        newRest              = removeEvent offEvent rest
        musicEvent           = makeEvent time instr ptch offTime
    in musicEvent : midiToMusicEvent instr newRest

-- for a certain noteOn event find the corresponding noteOff event + how many ticks after it triggers
-- note: no [] case it should always find the noteOff event
lookAhead :: AbsPitch -> [MidiEvent] -> (MidiEvent, Ticks)
lookAhead ptch events = lookAheadWithTicks ptch events 0
    where lookAheadWithTicks ptch (e@(t, NoteOff _ pitch _) : es) ticks
            | ptch == pitch = (e, t + ticks)
            | otherwise     = lookAheadWithTicks ptch es (t + ticks)
          lookAheadWithTicks ptch ((t, _) : es) ticks = lookAheadWithTicks ptch es (t + ticks)

-- turn ticks into realtive beats
-- note: 1 beat = 1 whole note
ticksToTime :: Ticks -> PTime
ticksToTime ticks = fromIntegral ticks / (2.0 * fromIntegral division)

-- remove a midi event from a list
removeEvent :: MidiEvent -> [MidiEvent] -> [MidiEvent]
removeEvent event [] = []
removeEvent event (e : es) 
    | e == event = es
    | otherwise  = e : removeEvent event es 

-- make a music event
makeEvent :: Double -> Instrument -> AbsPitch -> Duration -> MusicEvent
makeEvent time instr ptch dur = MEvent time instr ptch dur defVolume

-- turn a performance from relative time (time since the last note) 
-- to absolute time (time since the beginning of the piece)
fromRelativeTime :: Performance -> Performance
fromRelativeTime events = accTime events 0
    where accTime [] time = []
          accTime (e : es) time = let newTime = eTime e + time
                                  in e {eTime = newTime} : accTime es newTime