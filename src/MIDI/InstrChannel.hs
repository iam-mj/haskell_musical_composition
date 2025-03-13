module MIDI.InstrChannel where
-- definitions of concepts used in the association of an instrument to a Midi channel

import Music.Data
import Codec.Midi (Channel)

type ProgramNumber = Int
type InstrumentChannelMap = [(Instrument, Channel)]

-- given a list of instruments, associate each of them a channel (in a liniar fashion)
-- won't work for more than 15 instruments - dynamic association is not implemented
-- also care is taken so that channel 9 is skipped
makeICMap :: [Instrument] -> InstrumentChannelMap
makeICMap instrs = if length instrs > 15 
                        then error "Can't create a channel association relation for more than 15 instruments"
                        else makeICM instrs 0 []
    where makeICM [] ch icmap = icmap
          makeICM (i : is) ch icmap = makeICM is (if ch /= 8 then ch + 1 else ch + 2) ((i, ch) : icmap)

lookupChannel :: InstrumentChannelMap -> Instrument -> (Channel, ProgramNumber)
lookupChannel = undefined
