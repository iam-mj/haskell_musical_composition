module MIDI.ToMIDI where

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
type PTime = Float
type DurT = Float

defVolume = 64 :: Volume
noParams = [] :: [Double]
emptyPitch = 0 :: AbsPitch

-- TODO: add context at one point + tempo & metro


-- TODO: check this is correct
absPitch :: Pitch -> Octave -> AbsPitch
absPitch pit octave = octave * 12 + pitchToInt pit

-- TODO: make sure the timestamp updates as it should
-- TODO: write this using the do notation of the List Monad??
perform :: Music -> Performance
perform (Music trackE octave instr) = 
    let perf trE oct ins = case trE of
            EmptyET                 -> [MEvent 0 ins 0 0 0 []]
            PrimET (Note pit dur)   -> [MEvent 0 ins (absPitch pit oct) dur defVolume noParams]
            PrimET (Rest dur)       -> [MEvent 0 ins emptyPitch dur defVolume noParams]
            trackE1 :++: trackE2    -> perf trackE1 oct ins ++ perf trackE2 oct ins
            trackE1 :::: trackE2    -> undefined -- TODO: how do i do this...
    in perf trackE octave instr

