module Music.Utils where

import Music.Data

-- works as expected, but lots of paranteses are printed
link :: [Group] -> Track
link [] = EmptyT
link groups = foldl (\groups group -> groups :+: Prim group) (Prim (head groups)) (tail groups)

linkT :: [Track] -> Track
linkT [] = EmptyT
linkT tracks = foldl1 (:+:) tracks

-- repeat a track a number of times
repeatT :: Int -> Track -> Track
repeatT n track = linkT $ replicate n track

-- add to a track a sequence of repeating the same group / the same note
repeatG :: Int -> Group -> Track -> Track
repeatG n group track = track :+: link (replicate n group)

repeatN :: Int -> Primitive -> Track -> Track
repeatN n note = repeatG n (Single note)

-- get primitive duration
durationP :: Primitive -> Duration
durationP (Note _ dur _) = dur
durationP (Rest dur) = dur

-- get duration of a group
durationG :: Group -> Duration
durationG (Single primitive) = durationP primitive
durationG (Duo _ primitive) = durationP primitive
durationG (Chord _ primitive) = durationP primitive

-- get duration of a track
durationT :: Track -> Duration
durationT EmptyT = 0
durationT (Prim group) = durationG group
durationT (track1 :+: track2) = durationT track1 + durationT track2

-- get duration of an extended track
durationET :: TrackE -> Duration
durationET EmptyET = 0
durationET (PrimET (Note _ dur _)) = dur
durationET (PrimET (Rest dur)) = dur
durationET (track1 :++: track2) = durationET track1 + durationET track2
durationET (track1 :::: track2) = max (durationET track1) (durationET track2) 

-- parallelize two tracks
-- parallelizeT :: Track -> Track -> Track
-- parallelizeT EmptyT track = track
-- parallelizeT track EmptyT = track

-- group notes of different durations
-- parallelize :: [Primitive] -> Track 
-- parallelize [] = EmptyT
-- parallelize [note@(Note _ _)] = Prim (Single note)
-- parallelize ()

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

-- turn an int into a pitch class and signal an octave change
pitch :: Int -> OctaveChange -> (Pitch, OctaveChange)
pitch n change = let pitches = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
                     increase = n `div` 12
          in (pitches !! (n `mod` 12), change + increase)

-- increase a note's pitch by n semitones
-- if already an IncreaseOctave, gotta preserve it 
transpose :: Int -> Primitive -> Primitive
transpose n (Rest dur) = Rest dur -- no effect on a rest
transpose n (Note ptch dur change) = let (newPitch, newChange) = pitch (pitchToInt ptch + n) change
                                     in Note newPitch dur newChange

-- get the intervals in semitones for a certain chord
intervals :: Chord -> [Int]
intervals MajorThird = [0, 4, 7]
intervals MinorThird = [0, 3, 7]
intervals (CustomChord interval) = interval

-- interpret a Track as a TrackE in order to be able to play it
interpret :: Track -> TrackE
interpret track = interpretT $ cleanT track

-- TODO: don't export this one
interpretT :: Track -> TrackE
interpretT EmptyT = EmptyET
interpretT (Prim (Single prim)) = PrimET prim
interpretT (Prim (Duo int note)) = PrimET note :::: PrimET (transpose int note) 
interpretT (Prim (Chord chord note)) = foldl (\notes int -> notes :::: PrimET (transpose int note)) (PrimET note) (tail $ intervals chord)
interpretT (track1 :+: track2) = interpretT track1 :++: interpretT track2
