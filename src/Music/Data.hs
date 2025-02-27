module Music.Data where

-- TODO: consider not exporting the constructors, just the constructor functions
-- because only through the constructor functions can i check that certain conditions are met

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

type Octave = Int -- < 12

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
duo interval note@(Note _ _) = Duo interval note
duo _ _ = error "Can't make an interval with a rest root. Use a note instead or build a single."

chord :: Chord -> Primitive -> Group
chord chd note@(Note _ _) = Chord chd note
chord _ _ = error "Can't make chords with a rest root. Use a note instead or build a single."

track :: Group -> Track
track = Prim

music :: [TrackE] -> Octave -> Music
music tracks octave 
    | 0 <= octave && octave < 12 = foldl (\music track -> music ::: Music track octave) (Music (head tracks) octave) (tail tracks)
    | otherwise = error "Input octave is invalid. Make sure octave is a natural number and 0 <= octave < 12."
