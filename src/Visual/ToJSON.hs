module Visual.ToJSON where

import Data.Aeson
import Music.Data (Track (..), Group (..), Primitive (..), Chord)
import Music.Utils (unlink, pitchToInt, intervals)
import Data.Aeson.Key (fromString)

instance ToJSON Track where
    toJSON track = object [fromString "groups" .= map toJSON (unlink track)]

instance ToJSON Group where
    toJSON (Single note@(Note {}))  = object [fromString "type" .= "note",  fromString "root" .= toJSON note, fromString "semitones" .= Null]
    toJSON (Single rest@(Rest {}))  = object [fromString "type" .= "rest",  fromString "root" .= toJSON rest, fromString "semitones" .= Null]
    toJSON (Duo int prim)           = object [fromString "type" .= "duo",   fromString "root" .= toJSON prim, fromString "semitones" .= [int]]
    toJSON (Chord ch prim)          = object [fromString "type" .= "chord", fromString "root" .= toJSON prim, fromString "semitones" .= toJSON ch]

instance ToJSON Chord where
    toJSON chord = toJSON $ intervals chord

instance ToJSON Primitive where
    toJSON (Note ptch dur octch) = object [fromString "duration" .= dur, fromString "pitch" .= pitchToInt ptch, fromString "octaveChange" .= octch]
    toJSON (Rest dur)            = object [fromString "duration" .= dur, fromString "pitch" .= Null,            fromString "octaveChange" .= Null]