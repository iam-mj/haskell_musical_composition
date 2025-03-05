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
-- for custom chords: give the list of semitones describing your chord
-- e.g. MajorThid = [0, 4, 7]
data Chord = MajorThird | MinorThird | CustomChord [Int]
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
            | TrackE :::: TrackE -- parallel operator
            deriving Show

type Octave = Int -- < 12

-- from the midi standard
data Instrument = 
     AcousticGrandPiano     | BrightAcousticPiano    | ElectricGrandPiano
  |  HonkyTonkPiano         | RhodesPiano            | ChorusedPiano
  |  Harpsichord            | Clavinet               | Celesta 
  |  Glockenspiel           | MusicBox               | Vibraphone  
  |  Marimba                | Xylophone              | TubularBells
  |  Dulcimer               | HammondOrgan           | PercussiveOrgan 
  |  RockOrgan              | ChurchOrgan            | ReedOrgan
  |  Accordion              | Harmonica              | TangoAccordion
  |  AcousticGuitarNylon    | AcousticGuitarSteel    | ElectricGuitarJazz
  |  ElectricGuitarClean    | ElectricGuitarMuted    | OverdrivenGuitar
  |  DistortionGuitar       | GuitarHarmonics        | AcousticBass
  |  ElectricBassFingered   | ElectricBassPicked     | FretlessBass
  |  SlapBass1              | SlapBass2              | SynthBass1   
  |  SynthBass2             | Violin                 | Viola  
  |  Cello                  | Contrabass             | TremoloStrings
  |  PizzicatoStrings       | OrchestralHarp         | Timpani
  |  StringEnsemble1        | StringEnsemble2        | SynthStrings1
  |  SynthStrings2          | ChoirAahs              | VoiceOohs
  |  SynthVoice             | OrchestraHit           | Trumpet
  |  Trombone               | Tuba                   | MutedTrumpet
  |  FrenchHorn             | BrassSection           | SynthBrass1
  |  SynthBrass2            | SopranoSax             | AltoSax 
  |  TenorSax               | BaritoneSax            | Oboe  
  |  Bassoon                | EnglishHorn            | Clarinet
  |  Piccolo                | Flute                  | Recorder
  |  PanFlute               | BlownBottle            | Shakuhachi
  |  Whistle                | Ocarina                | Lead1Square
  |  Lead2Sawtooth          | Lead3Calliope          | Lead4Chiff
  |  Lead5Charang           | Lead6Voice             | Lead7Fifths
  |  Lead8BassLead          | Pad1NewAge             | Pad2Warm
  |  Pad3Polysynth          | Pad4Choir              | Pad5Bowed
  |  Pad6Metallic           | Pad7Halo               | Pad8Sweep
  |  FX1Train               | FX2Soundtrack          | FX3Crystal
  |  FX4Atmosphere          | FX5Brightness          | FX6Goblins
  |  FX7Echoes              | FX8SciFi               | Sitar
  |  Banjo                  | Shamisen               | Koto
  |  Kalimba                | Bagpipe                | Fiddle 
  |  Shanai                 | TinkleBell             | Agogo  
  |  SteelDrums             | Woodblock              | TaikoDrum
  |  MelodicDrum            | SynthDrum              | ReverseCymbal
  |  GuitarFretNoise        | BreathNoise            | Seashore
  |  BirdTweet              | TelephoneRing          | Helicopter
  |  Applause               | Gunshot                | Percussion
  deriving Show

-- adds a key to a track in order to make it playable
-- as well as a instrument
data Music = Music TrackE Octave Instrument
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

music :: [TrackE] -> Octave -> Instrument -> Music
music tracks octave instrument
    | 0 <= octave && octave < 12 = let f music track = music ::: Music track octave instrument
                                   in foldl f (Music (head tracks) octave instrument) (tail tracks)
    | otherwise = error "Input octave is invalid. Make sure octave is a natural number and 0 <= octave < 12."
