module MIDI.ToMIDI where

import MIDI.Performance
import MIDI.InstrChannel
import Codec.Midi
import Music.Data (Instrument, Music)

exportMusic :: Music -> FilePath -> IO ()
exportMusic m path = (exportFile path . toMidi . perform) m

-- can't make them directly synthetiser-midi values cause we might want to export them
-- turns a performance into a midi value: Midi fileType timeDev tracks
toMidi :: Performance -> Midi
toMidi performance = 
    let pairs = splitByInst performance
        icmap = makeICMap (map fst pairs)
    in Midi (if length pairs == 1 
                then SingleTrack 
                else MultiTrack)
            (TicksPerBeat division)
            (map (fromAbsTime . makeTrack icmap) pairs) -- TODO: what exactly does fromAbsTime doooo

division = 96 :: Int -- TODO: find out more about this

-- splits a performance into an association list of instruments and the events which are played by them
-- TODO: test that the events are ordered by timestamp as much as possible !! will lose performance wise :(
--       but still probably necessary :(
splitByInst :: Performance -> [(Instrument, [MusicEvent])]
splitByInst [] = []
splitByInst perf = split perf []
    where split [] pairs               = pairs 
          split (event : events) pairs = let inst = eInst event
                                             instEvents = lookup inst pairs
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

defTempo = 500000 :: Int

-- we'll use the NoteOn, NoteOff, ProgramChange, TempoChange & TrackEnd
-- we'll have a track for each instrument
makeTrack :: InstrumentChannelMap -> (Instrument, [MusicEvent]) -> [(Ticks, Message)]
makeTrack icmap (inst, events) = 
    let (ch, prgNum) = lookupChannel icmap inst
        instrEvent = (0, ProgramChange ch prgNum)
        tempoEvent = (0, TempoChange defTempo)
        endEvent = (0, TrackEnd) -- TODO: timestamp 0 is okay i wonder...
        melody [] = []
        melody (ev : evs) = let (start, stop) = makeMEvents ch ev
                            in start : insertMEvent stop (melody evs)
    in instrEvent : tempoEvent : melody events ++ [endEvent]

-- each MusicEvent will transform into 2 Midi events: one NoteOn & one NoteOff
makeMEvents :: Channel -> MusicEvent -> ((Ticks, Message), (Ticks, Message))
makeMEvents ch (MEvent {eTime = t, ePitch = pth, 
                        eDur = dur, eVol = v}) = ((toDelta t, NoteOn ch pth (limit v)),
                                                  (toDelta (t + dur), NoteOff ch pth (limit v)))
    where toDelta t = round (t * 2.0 * fromIntegral division) -- TODO: take better notes on toDelta
          limit v = min 0 (max 127 (fromIntegral v))

-- insert a midi event into a list of midi events so that the timestamps are in ascending order
insertMEvent :: (Ticks, Message) -> [(Ticks, Message)] -> [(Ticks, Message)]
insertMEvent e [] = [e]
insertMEvent e@(t, _) (e1@(t1, _) : es) = if t < t1 then e : e1 : es
                                                    else e1 : insertMEvent e es
