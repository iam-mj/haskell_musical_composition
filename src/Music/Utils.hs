module Music.Utils where

import Music.Data

-- works as expected, but lots of paranteses are printed
link :: [Group] -> Track
link [] = EmptyT
link groups = foldl (\groups group -> groups :+: Prim group) (Prim (head groups)) (tail groups)

linkT :: [Track] -> Track
linkT [] = EmptyT
linkT tracks = foldl1 (:+:) tracks

-- add to a track a sequence of repeating the same group / the same note
repeatG :: Track -> Group -> Int -> Track
repeatG track group n = track :+: link (replicate n group)

repeatN :: Track -> Primitive -> Int -> Track
repeatN track note = repeatG track (Single note)

-- remove all empty groups from a succesion of groups in a track
cleanT :: Track -> Track
cleanT EmptyT = EmptyT
cleanT track@(Prim group) = track
cleanT (EmptyT :+: groups) = cleanT groups
cleanT (groups :+: EmptyT) = cleanT groups
cleanT (groupsL :+: groupsR) = cleanT groupsL :+: cleanT groupsR

-- turn a pitch into an int
pitchToInt :: Pitch -> Int
pitchToInt pitch = case pitch of
    C -> 0; D -> 2; E -> 4; F -> 5; G -> 7; A -> 9; B -> 11;
    Cf -> -1; Df -> 1; Ef -> 3; Ff -> 4; Gf -> 6; Af -> 8; Bf -> 10;
    Cs -> 1; Ds -> 3; Es -> 5; Fs -> 6; Gs -> 8; As -> 10; Bs -> 12

-- turn an int into a pitch class
pitch :: Int -> Pitch
pitch n = let pitches = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
          in pitches !! (n `mod` 12)

-- TODO: does not change the octave!!
-- increase a note's pitch by n semitones
transpose :: Int -> Primitive -> Primitive
transpose n (Rest dur) = Rest dur -- no effect on a rest
transpose n (Note ptch dur) = Note (pitch $ pitchToInt ptch + n) dur

-- get the intervals in semitones for a certain chord
intervals :: Chord -> [Int]
intervals MajorThird = [0, 2, 4]
intervals MinorThird = [0, 2, 3]

-- interpret a Track as a TrackE in order to be able to play it
interpret :: Track -> TrackE
interpret EmptyT = EmptyET
interpret (Prim (Single prim)) = PrimET prim
interpret (Prim (Duo int note)) = PrimET note :::: PrimET (transpose int note) 
interpret (Prim (Chord chord note)) = foldl (\notes int -> notes :::: PrimET (transpose int note)) (PrimET note) (tail $ intervals chord)
interpret (track1 :+: track2) = interpret track1 :++: interpret track2
