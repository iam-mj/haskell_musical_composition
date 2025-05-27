module MIDI.FromMIDI where

import Codec.Midi
import Music.Data (Instrument, Music, Duration)
import MIDI.Performance
import MIDI.InstrChannel
import MIDI.Synthesizer
import MIDI.ToMIDI (MidiEvent)
import Data.Maybe (fromJust)

type Division = Int

loadMusic :: FilePath -> IO (Maybe Music)
loadMusic fileName = do
    result <- loadMidi fileName
    case result of
        Nothing   -> return Nothing
        Just midi -> do
            print midi
            return (Just $ unperform $ fromMidi midi)

-- transform a midi value into a performance
fromMidi :: Midi -> Performance
fromMidi midi = concatMap (trackToEvents (timeDiv midi)) (tracks midi)

-- transform a track to its corresponding music events
-- note: loaded imports usually have a first track of details, which should return no events
--       afterwards, the channel tracks have a ProgramChange event and an InstrumentName event
trackToEvents :: TimeDiv -> [MidiEvent] -> Performance
trackToEvents (TicksPerBeat division) ((_, ProgramChange _ pgrNum) : tempoOrInstrNameEvent : events) = 
    let instr = fromJust $ lookup pgrNum reverseGmsmap
    in fromRelativeTime $ midiToMusicEvent division instr events
trackToEvents _ _ = []

-- transform midi events into a performance's music events
-- note: any other midi events apart from note on / note off / trackend are ignored
-- note: need some placeholder events to maintain duration between all events (removed in fromRelativeTime)
midiToMusicEvent :: Division -> Instrument -> [MidiEvent] -> [MusicEvent]
midiToMusicEvent div instr [(_, TrackEnd)]          = []
midiToMusicEvent div instr ((ticks, NoteOff {}) : rest) = 
    let time             = ticksToTime div ticks
        placeholderEvent = emptyEvent { eTime = time }
    in placeholderEvent : midiToMusicEvent div instr rest
midiToMusicEvent div instr ((ticks, NoteOn _ ptch _) : rest) =
    let time       = ticksToTime div ticks
        offTime    = lookAhead div ptch rest
        musicEvent = makeEvent time instr ptch offTime
    in musicEvent : midiToMusicEvent div instr rest
midiToMusicEvent div instr ((_, _) : rest)          = midiToMusicEvent div instr rest

-- for a certain noteOn event find after how much time the corresponding noteOff event triggers
-- note: no [] case it should always find the noteOff event
lookAhead :: Division -> AbsPitch -> [MidiEvent] -> Time
lookAhead div ptch events = lookAheadWithTicks ptch events 0
    where lookAheadWithTicks ptch (e@(t, NoteOff _ pitch _) : es) ticks
            | ptch == pitch = ticksToTime div (t + ticks)
            | otherwise     = lookAheadWithTicks ptch es (t + ticks)
          lookAheadWithTicks ptch ((t, _) : es) ticks = lookAheadWithTicks ptch es (t + ticks)

-- turn ticks into realtive beats
-- note: 1 beat = 1 whole note
ticksToTime :: Division -> Ticks -> METime
ticksToTime division ticks = fromIntegral ticks / (4.0 * fromIntegral division)

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