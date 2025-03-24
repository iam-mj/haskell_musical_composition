module Main where

import Music.Data 
import Music.Utils
import MIDI.Performance

main :: IO ()
main = return ()

-- test values
myGroup = single $ note A 0.5
myDuo = duo 2 (note C 1)
myChord = chord MinorThird (note D 0.5)
myTrack = track myGroup

uncleanTrack = linkT [EmptyT, EmptyT, EmptyT, myTrack, EmptyT, EmptyT, myTrack, EmptyT]
cleanTrack = cleanT uncleanTrack

trackE = interpret $ link [myGroup, myDuo, myChord]

-- TODO: something weird happens if you try to play too many notes at the same time?
m1 = music [trackE] 4 Violin
m2 = music [trackE, trackE] 4 Violin
m3 = music [trackE, trackE, trackE] 4 Violin

p1 = perform m1
p2 = perform m2

-- let's try composing Merry-Go-Round of Life
-- TODO: they're supposed to be en, but would sound kinda bad :)
q1 = repeatT 3 $ link $ map (duo 2) [(note B qn), (note C qn)]
q2 = repeatT 3 $ link $ map (duo 2) [(note A qn), (note B qn)]
q3 = repeatT 3 $ link $ map (duo 2) [(note G qn), (note A qn)]
tr1 = linkT [q1, q2, q3]

mgr = music [interpret tr1] 4 AcousticGrandPiano