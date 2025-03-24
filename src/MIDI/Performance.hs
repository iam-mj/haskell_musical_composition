module MIDI.Performance where

import Music.Data
import Music.Utils

type AbsPitch = Int
-- TODO: set it somewhere! :)
type Volume = Int

-- each Music value ready to be played should be first transformed into 
-- an event list more closer to what MIDI files expect
data MusicEvent = MEvent {
    eTime :: PTime,           -- onset time
    eInst :: Instrument,      -- assigned instrument
    ePitch :: AbsPitch,       -- pitch number from 0–127
    eDur :: DurT,             -- note duration
    eVol :: Volume,           -- volume from 0–127
    eParams :: [Double]       -- optional other parameters (TODO: keeping them?)
    } deriving Show

type Performance = [MusicEvent]
type PTime = Double
type DurT = Double

defVolume = 64 :: Volume
noParams = [] :: [Double]
emptyPitch = 0 :: AbsPitch

-- TODO: add context at one point + tempo & metro


-- should always be between 0-127
-- middle C (C, 4) should be 60
absPitch :: Pitch -> Octave -> AbsPitch
absPitch pit octave = (octave + 1) * 12 + pitchToInt pit

-- merge 2 performances into 1, so that the events are in ascending order by the timestamp
merge :: Performance -> Performance -> Performance
merge p1 [] = p1
merge [] p2 = p2
merge p1@(e1 : es1) p2@(e2 : es2)
    | eTime e1 <= eTime e2 = e1 : merge es1 p2
    | otherwise            = e2 : merge p1 es2

-- transforms a music piece into a performance (<=> an event list)
perform :: Music -> Performance
perform (Music trackE octave instr) = 
    let perf trE oct ins time = case trE of
            EmptyET                 -> return $ MEvent time ins 0 0 0 []
            PrimET (Note pit dur)   -> return $ MEvent time ins (absPitch pit oct) dur defVolume noParams
            PrimET (Rest dur)       -> return $ MEvent time ins emptyPitch dur defVolume noParams
            trackE1 :++: trackE2    -> perf trackE1 oct ins time ++ perf trackE2 oct ins (time + durationET trackE1)
            trackE1 :::: trackE2    -> merge (perf trackE1 oct ins time) (perf trackE2 oct ins time)
    in perf trackE octave instr 0
perform (music1 ::: music2) = merge (perform music1) (perform music2)

