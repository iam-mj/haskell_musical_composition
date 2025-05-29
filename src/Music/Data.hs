module Music.Data where

type Duration = Double -- where a whole note has a 1.0 duration, half a note has 0.5 ...

-- the "pitchToInt" function depends on these constructors
data Pitch =  A  | B  | C  | D  | E  | F  | G
            | As | Bs | Cs | Ds | Es | Fs | Gs
            | Af | Bf | Cf | Df | Ef | Ff | Gf 
            deriving (Show, Eq)

type OctaveChange = Int

data Primitive = Note Pitch Duration OctaveChange | Rest Duration
                deriving (Show, Eq)

type Interval = Int -- in semitones

-- the "intervals" function depends on these constructors
-- for custom chords: give the list of semitones describing your chord
-- e.g. MajorThid = [0, 4, 7]
data Chord = MajorTriad | MinorTriad | DiminishedTriad | AugmentedTriad | CustomChord [Int]
            deriving Show

-- used for the operations availble to the end user
-- a group of notes sang in parallel
data Group = Single Primitive
            | Duo Interval Primitive -- root note + distance from it
            | Chord Chord Primitive  -- the type of chord + the root note
            deriving Show

data Track = EmptyT
            | Prim Group
            | Track :+: Track

-- user specific data structures get interpreted into more specific data types - we call these "extended"
data TrackE = EmptyET            -- nothing 
            | PrimET Primitive   -- note / rest
            | TrackE :++: TrackE -- sequence operator
            | TrackE :::: TrackE -- parallel operator
            deriving Eq

type Octave = Int -- >= -1 && <= 9

-- adds a key to a track in order to make it playable
-- as well as a instrument
data Music = Music TrackE Octave Instrument
            | Music ::: Music
            deriving Eq

------------- ERRRORS --------------------

type Error = String

data ConstrErr = RestInterval | RestChord | InvalidOctave
                deriving Eq 
            
errorMessages :: [(ConstrErr, Error)]
errorMessages = [
    (RestInterval,  "Error: Can't make an interval with a rest root. Use a note instead or build a single."),
    (RestChord,     "Error: Can't make chords with a rest root. Use a note instead or build a single."),
    (InvalidOctave, "Error: Input octave is invalid. Make sure octave is an integer between -1 and 9.")]


----- CONSTRUCTOR FUNCTIONS --------------

note :: Pitch -> Duration -> OctaveChange -> Primitive
note ptch dur ch = Note ptch dur (min (-10) . max 10 $ ch)
noteDef :: Pitch -> Duration -> Primitive
noteDef ptch dur = Note ptch dur 0
noteInc :: Pitch -> Duration -> Primitive
noteInc ptch dur = Note ptch dur 1
rest :: Duration -> Primitive
rest = Rest

single :: Primitive -> Group
single note@(Note {}) = Single note
single rest@(Rest _) = Single rest

duo :: Interval -> Primitive -> Either Error Group
duo interval note@(Note {}) = Right $ Duo interval note
duo _ _ = let Just err = lookup RestInterval errorMessages
          in Left err

chord :: Chord -> Primitive -> Either Error Group
chord chd note@(Note {}) = Right $ Chord chd note
chord _ _ = let Just err = lookup RestChord errorMessages
            in Left err

track :: Group -> Track
track = Prim

music :: [TrackE] -> Octave -> Instrument -> Either Error Music
music tracks octave instrument
    | -1 <= octave && octave <= 9 = let f music track = music ::: Music track octave instrument
                                    in Right $ foldl f (Music (head tracks) octave instrument) (tail tracks)
    | otherwise                   = let Just err = lookup InvalidOctave errorMessages
                                    in Left err


-- standard durations
bn, wn, hn, qn, en, sn, tn, sfn :: Duration
bn    = 2;     wn    = 1 
hn    = 1/2;   qn    = 1/4
en    = 1/8;   sn    = 1/16
tn    = 1/32;  sfn   = 1/64


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
  deriving (Show, Eq)