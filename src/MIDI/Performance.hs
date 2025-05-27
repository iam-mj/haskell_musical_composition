module MIDI.Performance where

import Music.Data
import Music.Utils

-- NOTE: the METime is held in relative beats, where 1 beat = 1 whole note

type METime    = Double
type AbsPitch  = Int
type Volume    = Int
type Performance = [MusicEvent]

type InstrSplit    = [(Instrument, Performance)]
type InstrOctSplit = [(Instrument, [(Octave, Performance)])]

-- each Music value ready to be played should be first transformed into 
-- an event list more closer to what MIDI files expect
-- note: volume always a default, but needed in the noteon/noteoff midi events
data MusicEvent = MEvent {
    eTime  :: METime,         -- onset time
    eInst  :: Instrument,     -- assigned instrument
    ePitch :: AbsPitch,       -- pitch number from 0–127
    eDur   :: Duration,       -- note duration
    eVol   :: Volume          -- volume from 0–127
    } deriving Show

defVolume  = 64 :: Volume
emptyPitch = 0  :: AbsPitch
emptyEvent = MEvent 0 noInstrument 0 0 0 :: MusicEvent

----------------------------------------------
--         MUSIC TO PERFORMANCE             --
----------------------------------------------

-- note: should always be between 0-127
-- note: middle C (C, 4) should be 60
absPitch :: Pitch -> Octave -> OctaveChange -> AbsPitch
absPitch pit octave change = max 0 . min 127 $ (octave + 1 + change) * 12 + pitchToInt pit

-- merge 2 performances into 1, so that the events are in ascending order by the timestamp
merge :: Performance -> Performance -> Performance
merge p1 [] = p1
merge [] p2 = p2
merge p1@(e1 : es1) p2@(e2 : es2)
    | eTime e1 <= eTime e2 = e1 : merge es1 p2
    | otherwise            = e2 : merge p1 es2

-- transforms a music piece into a performance (<=> an event list)
perform :: Music -> Performance
perform (Music trackE octave instr) = perf trackE octave instr 0
    where perf trE oct ins time = case trE of
            EmptyET                      -> return $ MEvent time ins 0 0 0
            PrimET (Note pit dur change) -> return $ MEvent time ins (absPitch pit oct change) dur defVolume
            PrimET (Rest dur)            -> return $ MEvent time ins emptyPitch dur defVolume
            trackE1 :++: trackE2         -> perf trackE1 oct ins time ++ perf trackE2 oct ins (time + durationET trackE1)
            trackE1 :::: trackE2         -> merge (perf trackE1 oct ins time) (perf trackE2 oct ins time)
perform (music1 ::: music2) = merge (perform music1) (perform music2)


----------------------------------------------
--         PERFORMANCE TO MUSIC             --
----------------------------------------------

-- transform a performance back into a music piece
unperform :: Performance -> Music
unperform events = let instrSplit    = splitByInstr events            -- split by instruments
                       instrOctSplit = splitInstrByOctave instrSplit  -- split again by octave
                   in removeEmpty $ splitToMusic instrOctSplit

-- remove the leftover empty music from splitToMusic
removeEmpty :: Music -> Music
removeEmpty music@(Music {}) = music
removeEmpty (music1 ::: music2)
    | music1 == emptyMusic = removeEmpty music2
    | music2 == emptyMusic = removeEmpty music1
    | otherwise            = removeEmpty music1 ::: removeEmpty music2

-- split the performance events by instrument
splitByInstr :: Performance -> InstrSplit
splitByInstr events = splitByInstr events []
    where splitByInstr [] list       = list
          splitByInstr (e : es) list =
            let instr       = eInst e
                instrEvents = lookup instr list
            in case instrEvents of
                Nothing     -> splitByInstr es ((instr, [e]) : list)
                Just events -> splitByInstr es (updateList instr (insertEvent e events) list)

-- insert a music event into a list of music events, keeping the timestamps in order
insertEvent :: MusicEvent -> [MusicEvent] -> [MusicEvent]
insertEvent event []        = [event]
insertEvent event (e : es) 
    | eTime event < eTime e = event : e : es
    | otherwise             = e : insertEvent event es

-- split performance events by octave
-- note: will have the actual octave in octave changeit
splitInstrByOctave :: InstrSplit -> InstrOctSplit
splitInstrByOctave instrSplit = splitOctave instrSplit []
    where splitOctave [] list = list
          splitOctave ((instr, events) : rest) list = splitOctave rest ((instr, makeOctaveSplit events []) : list)
          makeOctaveSplit [] list       = list
          makeOctaveSplit (e : es) list =
            let (_, oct)  = pitch (ePitch e) noChange
                octEvents = lookup oct list
            in case octEvents of
                Nothing     -> makeOctaveSplit es ((oct, [e]) : list)
                Just events -> makeOctaveSplit es (updateList oct (e : events) list)

-- transform a instrument & octave split into music
splitToMusic :: InstrOctSplit -> Music
splitToMusic []                       = emptyMusic
splitToMusic ((instr, events) : rest) = octSplitToMusic instr events ::: splitToMusic rest
    where octSplitToMusic instr []                     = emptyMusic
          octSplitToMusic instr ((oct, events) : rest) =
            let orderedEvents = orderEvents events
                track         = eventsToTrack orderedEvents
                cleanTrack    = (cleanET . cleanET) track
            in Music cleanTrack oct instr ::: octSplitToMusic instr rest

-- order events after the time they trigger
orderEvents :: Performance -> Performance
orderEvents list = makeOrdered list []
    where makeOrdered [] list       = list
          makeOrdered (e : es) list = makeOrdered es (insertEvent e list)

-- transform events to an extended track
-- note: three cases when making a track:
--    a) notes rendered at the same time
--    b) notes rendered one after the other
--    c) notes rendered one after the other but with some time between them (here, a rest is inserted)
-- note: lastTime = last starting time
--       lastDur  = duration of the latest "newStructure" carried around
eventsToTrack :: Performance -> TrackE
eventsToTrack []     = EmptyET
eventsToTrack events = makeTrack events 0 0 EmptyET EmptyET
    where makeTrack [] _ _ lastStructure track = track :++: lastStructure
          makeTrack (e : es) lastTime lastDur newStructure track
            | eTime e == lastTime           = let newDur           = max lastDur (eDur e)
                                                  updatedStructure = newStructure :::: toTrack e
                                              in makeTrack es lastTime newDur updatedStructure track
            | eTime e - lastTime == lastDur = makeTrack es (eTime e) (eDur e) (toTrack e) (track :++: newStructure)
            | otherwise                     = let timeDiff       = eTime e - lastTime - lastDur
                                                  insertRest dur = PrimET $ Rest dur
                                                  newTrack       = track :++: newStructure :++: insertRest timeDiff
                                              in makeTrack es (eTime e) (eDur e) (toTrack e) newTrack

-- transform one event back to a primitive
toTrack :: MusicEvent -> TrackE
toTrack event
    | ePitch event == 0 = PrimET $ Rest $ eDur event
    | otherwise         = let (ptch, _) = pitch (ePitch event) noChange
                          in PrimET $ Note ptch (eDur event) noChange