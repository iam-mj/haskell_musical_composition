module MIDI.ToMIDI where

import Codec.Midi
import Music.Data (Instrument, Music)
import MIDI.Performance
import MIDI.InstrChannel
import MIDI.Synthesizer

type MidiEvent       = (Ticks, Message)
type InstrumentTrack = (Instrument, [MusicEvent]) -- an instrument and the events which correspond to it

division = 96 :: Int -- note: ticks per beat (qn)

saveMusic :: Music -> FilePath -> IO ()
saveMusic music file = exportFile file ((toMidi . perform) music)

playMusic :: Music -> IO ()
playMusic = playMidi . toMidi . perform

musicToMidi :: Music -> Midi
musicToMidi = toMidi . perform

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

-- ticks after the last event after which comes the end of track
-- in order to prevent unwanted clipping
waitTillEnd = 256 :: Ticks

-- add the end of track event
addEnd :: [MidiEvent] -> [MidiEvent]
addEnd msgList = msgList ++ [(waitTillEnd, TrackEnd)]

-- make a track for each instrument
-- note: we'll use the NoteOn, NoteOff, ProgramChange, TempoChange
-- note: will ignore rests as they'll be added if needed at loading
makeTrack :: InstrumentChannelMap -> InstrumentTrack -> [MidiEvent]
makeTrack icmap (inst, events) = 
    let (ch, prgNum)   = lookupChannel icmap inst
        instrEvent     = (0, ProgramChange ch prgNum)
        tempoEvent     = (0, TempoChange defTempo)
        relevantEvents = filter (\e -> ePitch e > 0) events
        melody []         = []
        melody (ev : evs) = let (start, stop) = makeMEvents ch ev
                            in start : insertMEvent stop (melody evs)
    in instrEvent : tempoEvent : melody relevantEvents

-- each MusicEvent will transform into 2 Midi events: one NoteOn & one NoteOff
-- note: toDelta transforms relative beats (1 beat = 1 whole note) into ticks
makeMEvents :: Channel -> MusicEvent -> (MidiEvent, MidiEvent)
makeMEvents ch (MEvent {eTime = t, ePitch = pth, eDur = dur, eVol = v}) = 
    ((toDelta t, NoteOn ch pth (limit v)), (toDelta (t + dur), NoteOff ch pth (limit v)))
    where toDelta t = round (t * 4.0 * fromIntegral division) -- note: ticks = beats (wn) * ticks per beat (qn)
          limit v   = max 0 (min 127 v)

-- insert a midi event into a list of midi events so that the timestamps are in ascending order
insertMEvent :: MidiEvent -> [MidiEvent] -> [MidiEvent]
insertMEvent e [] = [e]
insertMEvent e@(t, _) (e1@(t1, _) : es) 
    | t <= t1   = e  : e1 : es 
    | otherwise = e1 : insertMEvent e es
