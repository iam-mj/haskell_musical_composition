module MIDI.FromMIDI where

import Codec.Midi
import Music.Data (Instrument, Music, Duration)
import MIDI.Performance
import MIDI.InstrChannel
import MIDI.Synthesizer
import MIDI.ToMIDI (MidiEvent, division)
import Data.Maybe (fromJust)

-- TODO: TEST 1 - make sure the first events really are the instrument + tempo sets

loadMusic :: FilePath -> IO (Maybe Music)
loadMusic fileName = do
    result <- loadMidi fileName
    case result of
        Nothing   -> return Nothing
        Just midi -> return (Just $ unperform $ fromMidi midi)

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

-- transform midi events into a performance's music events
-- note: any other midi events apart from note on / note off / trackend are ignored
-- note: need some placeholder events to maintain duration between all events (removed in fromRelativeTime)
midiToMusicEvent :: Instrument -> [MidiEvent] -> [MusicEvent]
midiToMusicEvent instr [(_, TrackEnd)]          = []
midiToMusicEvent instr ((ticks, NoteOff {}) : rest) = 
    let time             = ticksToTime ticks
        placeholderEvent = emptyEvent { eTime = time }
    in placeholderEvent : midiToMusicEvent instr rest
midiToMusicEvent instr ((ticks, NoteOn _ ptch _) : rest) =
    let time       = ticksToTime ticks
        offTime    = lookAhead ptch rest
        musicEvent = makeEvent time instr ptch offTime
    in musicEvent : midiToMusicEvent instr rest
midiToMusicEvent instr ((_, _) : rest)          = midiToMusicEvent instr rest

-- for a certain noteOn event find after how much time the corresponding noteOff event triggers
-- note: no [] case it should always find the noteOff event
lookAhead :: AbsPitch -> [MidiEvent] -> Time
lookAhead ptch events = lookAheadWithTicks ptch events 0
    where lookAheadWithTicks ptch (e@(t, NoteOff _ pitch _) : es) ticks
            | ptch == pitch = ticksToTime (t + ticks)
            | otherwise     = lookAheadWithTicks ptch es (t + ticks)
          lookAheadWithTicks ptch ((t, _) : es) ticks = lookAheadWithTicks ptch es (t + ticks)

-- turn ticks into realtive beats
-- note: 1 beat = 1 whole note
ticksToTime :: Ticks -> PTime
ticksToTime ticks = fromIntegral ticks / (2.0 * fromIntegral division)

-- remove a midi event from a list
-- removeEvent :: MidiEvent -> [MidiEvent] -> [MidiEvent]
-- removeEvent event [] = []
-- removeEvent event (e : es) 
--     | e == event = es
--     | otherwise  = e : removeEvent event es 

-- make a music event
makeEvent :: Double -> Instrument -> AbsPitch -> Duration -> MusicEvent
makeEvent time instr ptch dur = MEvent time instr ptch dur defVolume

-- turn a performance from relative time (time since the last note) 
-- to absolute time (time since the beginning of the piece)
-- also removes the placeholder events inserted in a prior step (see midiToMusicEvents)
-- note: resulted performance events will be ordered after time
fromRelativeTime :: Performance -> Performance
fromRelativeTime events = accTime events 0
    where accTime [] time = []
          accTime (e : es) time 
            | ePitch e == 0 = accTime es (eTime e + time)
            | otherwise     = let newTime = eTime e + time
                              in e {eTime = newTime} : accTime es newTime