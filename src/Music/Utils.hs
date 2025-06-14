module Music.Utils where

import Music.Data

noChange = 0 :: OctaveChange
noInstrument = AcousticGrandPiano :: Instrument
emptyMusic = Music EmptyET 0 noInstrument

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
durationP (Rest dur)     = dur

-- get duration of a group
durationG :: Group -> Duration
durationG (Single primitive)  = durationP primitive
durationG (Duo _ primitive)   = durationP primitive
durationG (Chord _ primitive) = durationP primitive

-- get duration of a track
durationT :: Track -> Duration
durationT EmptyT              = 0
durationT (Prim group)        = durationG group
durationT (track1 :+: track2) = durationT track1 + durationT track2

-- get duration of an extended track
durationET :: TrackE -> Duration
durationET EmptyET                 = 0
durationET (PrimET (Note _ dur _)) = dur
durationET (PrimET (Rest dur))     = dur
durationET (track1 :++: track2)    = durationET track1 + durationET track2
durationET (track1 :::: track2)    = max (durationET track1) (durationET track2)

-- get the list of groups inside a track
unlink :: Track -> [Group]
unlink EmptyT                   = []
unlink (Prim single@(Single _)) = [single]
unlink (Prim duo@(Duo _ _))     = [duo]
unlink (Prim chord@(Chord _ _)) = [chord]
unlink (track1 :+: track2)      = unlink track1 ++ unlink track2

lengthT :: Track -> Int
lengthT = length . unlink 

lengthET :: TrackE -> Int
lengthET EmptyET    = 0
lengthET (PrimET _) = 1
lengthET (track1 :++: track2) = lengthET track1 + lengthET track2
lengthET (track1 :::: track2) = lengthET track1 + lengthET track2

lengthM :: Music -> Int
lengthM (Music trackE _ _)  = lengthET trackE
lengthM (music1 ::: music2) = lengthM music1 + lengthM music2

-- insert a track inside another track at a certain position
insertT :: Track -> Int -> Track -> Track
insertT track idx insertTr =
    let groupsTrack  = unlink track
        groupsInsert = unlink insertTr
    in link $ insertG groupsTrack idx groupsInsert 0
    where
        insertG [] _ groupsInsert _ = groupsInsert
        insertG (g : gs) idx groupsInsert n
            | n == idx  = groupsInsert ++ (g : gs)
            | otherwise = g : insertG gs idx groupsInsert (n + 1)

-- delete one group from a list of groups 
deleteG1 :: [Group] -> Int -> Int -> [Group]
deleteG1 [] _ _ = []
deleteG1 (g : gs) idx n
    | idx == n  = gs
    | otherwise = g : deleteG1 gs idx (n + 1)

-- delete a series of consecutive groups from a list
deleteGM :: [Group] -> Int -> Int -> Int -> [Group]
deleteGM [] _ _ _ = []
deleteGM (g : gs) left right n
    | left <= n && n <= right = deleteGM gs left right (n + 1)
    | n > right               = g : gs
    | otherwise               = g : deleteGM gs left right (n + 1)

-- delete the groups inside a track at certain indexes
deleteT :: Track -> (Int, Maybe Int) -> Track
deleteT track (l, r) = let groups = case r of
                                Nothing -> deleteG1 (unlink track) l 0
                                Just r  -> deleteGM (unlink track) l r 0
                       in link groups

-- replace the groups inside a track at certain indexes with the groups of another track
replaceT :: Track -> (Int, Maybe Int) -> Track -> Track
replaceT track (l, r) replaceTr = let afterDeleteTr = deleteT track (l, r)
                                  in insertT afterDeleteTr l replaceTr

-- add a track a number of times to another track
addRepeatT :: Track -> Track -> Maybe Int -> Track
addRepeatT track addTr num = case num of
                                Nothing -> track :+: addTr
                                Just nr -> track :+: repeatT nr addTr


updateList :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
updateList name newValue [] = []
updateList name newValue (currentPair@(name', value) : rest)
    | name' == name = (name, newValue) : rest
    | otherwise     = currentPair      : updateList name newValue rest

flattenM :: Music -> [Pitch] -> Music
flattenM (Music trackE oct inst) ptchs = Music (flattenET trackE ptchs) oct inst
flattenM (music1 ::: music2) ptchs     = flattenM music1 ptchs ::: flattenM music2 ptchs

flattenET :: TrackE -> [Pitch] -> TrackE
flattenET = foldl flatten
    where flatten EmptyET _           = EmptyET
          flatten (PrimET prim) ptch  = PrimET $ flattenP prim ptch
          flatten (tr1 :++: tr2) ptch = flatten tr1 ptch :++: flatten tr2 ptch
          flatten (tr1 :::: tr2) ptch = flatten tr1 ptch :::: flatten tr2 ptch

flattenT :: Track -> [Pitch] -> Track
flattenT = foldl flatten
    where flatten EmptyT _           = EmptyT
          flatten (Prim group) ptch  = Prim $ flattenG group ptch
          flatten (tr1 :+: tr2) ptch = flatten tr1 ptch :+: flatten tr2 ptch

flattenG :: Group -> Pitch -> Group
flattenG (Single prim) ptch   = Single $ flattenP prim ptch
flattenG (Duo int prim) ptch  = Duo int $ flattenP prim ptch
flattenG (Chord ch prim) ptch = Chord ch $ flattenP prim ptch

flattenP :: Primitive -> Pitch -> Primitive
flattenP rest@(Rest _) _            = rest
flattenP note@(Note nptch dur octch) ptch
    | nptch /= ptch = note
    | otherwise     = transpose (-1) note

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
cleanT EmptyT                = EmptyT
cleanT track@(Prim group)    = track
cleanT (EmptyT :+: groups)   = cleanT groups
cleanT (groups :+: EmptyT)   = cleanT groups
cleanT (groupsL :+: groupsR) = cleanT groupsL :+: cleanT groupsR

cleanET :: TrackE -> TrackE
cleanET EmptyET                = EmptyET
cleanET track@(PrimET _)       = track
cleanET (EmptyET :++: groups)  = cleanET groups
cleanET (groups :++: EmptyET)  = cleanET groups
cleanET (groupsL :++: groupsR) = cleanET groupsL :++: cleanET groupsR
cleanET (EmptyET :::: groups)  = cleanET groups
cleanET (groups :::: EmptyET)  = cleanET groups 
cleanET (groupsL :::: groupsR) = cleanET groupsL :::: cleanET groupsR

-- turn a pitch into an int
pitchToInt :: Pitch -> Int
pitchToInt pitch = case pitch of
    C  ->  0;  D -> 2;  E -> 4;  F -> 5;  G -> 7;  A -> 9;   B -> 11;
    Cf -> -1; Df -> 1; Ef -> 3; Ff -> 4; Gf -> 6; Af -> 8;  Bf -> 10;
    Cs ->  1; Ds -> 3; Es -> 5; Fs -> 6; Gs -> 8; As -> 10; Bs -> 12

-- turn an int into a pitch class and signal an octave change
-- note: the octave change will give an octave as low as -1 as per the note names in English
pitch :: Int -> OctaveChange -> (Pitch, OctaveChange)
pitch n change = let pitches = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
                     increase = n `div` 12
          in (pitches !! (n `mod` 12), change + increase)

-- increase a note's pitch by n semitones
-- if already an IncreaseOctave, gotta preserve it 
transpose :: Int -> Primitive -> Primitive
transpose n (Rest dur) = Rest dur -- no effect on a rest
transpose n (Note ptch dur change) = let (newPitch, newChange) = pitch (pitchToInt ptch + n) change
                                     in note newPitch dur newChange

-- transpose a group with a number of semintones
transposeG :: Int -> Group -> Group
transposeG num (Single prim)   = Single $ transpose num prim
transposeG num (Duo int prim)  = Duo int $ transpose num prim
transposeG num (Chord ch prim) = Chord ch $ transpose num prim

-- transpose a track with a number of semitones
transposeT :: Track -> Int -> Track
transposeT track num = link $ map (transposeG num) $ unlink track

-- transpose an extended track with a number of semitones
transposeTE :: TrackE -> Int -> TrackE
transposeTE EmptyET _          = EmptyET
transposeTE (PrimET prim) num  = PrimET $ transpose num prim
transposeTE (tr1 :++: tr2) num = transposeTE tr1 num :++: transposeTE tr2 num
transposeTE (tr1 :::: tr2) num = transposeTE tr1 num :::: transposeTE tr2 num

-- transpose music with a number of semitones
transposeM :: Music -> Int -> Music
transposeM (Music track oct inst) num = Music (transposeTE track num) oct inst
transposeM (music1 ::: music2) num    = transposeM music1 num ::: transposeM music2 num

-- get the intervals in semitones for a certain chord
intervals :: Chord -> [Int]
intervals MajorTriad      = [0, 4, 7]
intervals MinorTriad      = [0, 3, 7]
intervals DiminishedTriad = [0, 3, 6]
intervals AugmentedTriad  = [0, 4, 8] 
intervals (CustomChord intervals) = intervals

-- interpret a Track as a TrackE in order to be able to play it
interpret :: Track -> TrackE
interpret track = interpretT $ cleanT track
    where
        interpretT EmptyT                    = EmptyET
        interpretT (Prim (Single prim))      = PrimET prim
        interpretT (Prim (Duo int note))     = PrimET note :::: PrimET (transpose int note)
        interpretT (Prim (Chord chord note)) = foldl (\notes int -> notes :::: PrimET (transpose int note)) (PrimET note) (tail $ intervals chord)
        interpretT (track1 :+: track2)       = interpretT track1 :++: interpretT track2
