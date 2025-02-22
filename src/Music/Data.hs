module Music.Data where

type Duration = Float -- where a whole note has a 1.0 duration, half a note has 0.5 ...

-- the "pitchToInt" function depends on these constructors
data Pitch = A | B | C | D | E | F | G -- TODO: add more accessories?
            | As | Bs | Cs | Ds | Es | Fs | Gs
            | Af | Bf | Cf | Df | Ef | Ff | Gf 
            deriving Show

data Primitive = Note Pitch Duration | Rest Duration
                deriving Show

type Interval = Int -- in semitones

-- the "intervals" function depends on these constructors
data Chord = MajorThird | MinorThird -- TODO: add more
            deriving Show

-- used for the operations availble to the end user
-- a group of notes sang in parallel
data Group = Single Primitive
            | Duo Interval Primitive -- root note + distance from it
            | Chord Chord Primitive -- the type of chord + the root note
            deriving Show

data Track = EmptyT
            | Prim Group
            | Track :+: Track
            deriving Show
            -- TODO: maybe constumize the show method? get rid of all the ()?

-- user specific data structures get interpreted into more specific data types - we call these "extended"
data TrackE = EmptyET            -- nothing 
            | PrimET Primitive   -- note / rest
            | TrackE :++: TrackE -- sequence operator
            | TrackE :::: TrackE  -- parallel operator
            deriving Show

type Octave = Int -- <= 12 ?? - might have to check this at creation !!

-- adds a key to a track in order to make it playable
data Music = Music TrackE Octave
            | Music ::: Music
            deriving Show


-- constructor functions
note :: Pitch -> Duration -> Primitive
note = Note
rest :: Duration -> Primitive
rest = Rest

single :: Primitive -> Group
single note@(Note _ _) = Single note
single rest@(Rest _) = Single rest

duo :: Interval -> Primitive -> Group
duo interval note@(Note _ _)= Duo interval note
-- TODO: duo _ _ = "Error - can't make interval with rest root; use a note instead or try a single"

chord :: Chord -> Primitive -> Group
chord chd note@(Note _ _) = Chord chd note
-- TODO: chord _ _ = "Error - can't make chords with rest root; use a note instead or try a single"

track :: Group -> Track
track = Prim

-- music :: TrackE -> Octave -> Either String Music
-- music track octave
--     | 0 < octave && octave <= 12 = Right (Music track octave)
--     | otherwise = Left "Given octave is invalid. Please make sure the octave is greater than 0 and less than 13."

-- TODO: manage octave exception
music :: [TrackE] -> Octave -> Music
music tracks octave = foldl (\music track -> music ::: Music track octave) (Music (head tracks) octave) (tail tracks)
