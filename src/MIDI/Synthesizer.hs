module MIDI.Synthesizer where

import Sound.PortMidi
import Codec.Midi
import Control.Concurrent (threadDelay)
import Data.List (sortOn)
import Data.Maybe (fromJust)

defTempo = 500000 :: Int

openDevice :: IO (Either PMError PMStream)
openDevice = do
    maybeDevice <- getDefaultOutputDeviceID
    case maybeDevice of 
        Nothing       -> do
            putStrLn "Error getting the default output device"
            return (Left InvalidDeviceId)
        Just deviceId -> do
            open <- openOutput deviceId 10
            return open

-- render audio of a given midi file
playMidiFile :: FilePath -> IO ()
playMidiFile midiFile = do
    initialize
    open <- openDevice
    case open of
        Left err     -> putStrLn $ "Error opening the default device" -- TODO: print error somehow
        Right stream -> do
            result <- importFile midiFile
            case result of
                Left err   -> putStrLn $ "Error loading MIDI file: " ++ err
                Right midi -> sendMidiEvents stream (getPPQ midi) (concat (tracks midi))
            _ <- close stream
            return ()
    _ <- terminate
    return ()

playMidi :: Midi -> IO ()
playMidi midi = do
    initialize
    open <- openDevice
    case open of
        Left err     -> putStrLn $ "Error opening the default device" -- TODO: print error somehow
        Right stream -> do
            sendMidiEvents stream (getPPQ midi) (concat (tracks midi)) -- TODO: this concat tracks won't work with more than one track!!
            _ <- close stream
            return ()
    _ <- terminate
    return ()

defPPQ = 480 :: Int

getPPQ :: Midi -> Int
getPPQ midi = case timeDiv midi of
                TicksPerBeat x -> x
                _              -> defPPQ

-- send midi events to an output device
sendMidiEvents :: PMStream -> Int -> [(Ticks, Message)] -> IO ()
sendMidiEvents stream ppq events = sendMidi events defTempo 0
  where
    sendMidi [] _ _ = return ()
    sendMidi ((ticks, msg) : msgs) tempo lastTime = 
        let timeDiff = ticksToMicroseconds ticks tempo ppq - lastTime
            newTempo = case msg of
                TempoChange differentTempo -> differentTempo
                _                          -> tempo
        in do
            threadDelay (max 0 timeDiff)  -- wait before sending next event
            case toPMMsg msg of
                Just pmmsg -> writeShort stream (PMEvent (encodeMsg pmmsg) 0)
                Nothing    -> return (Right NoError'NoData)
            sendMidi msgs newTempo (lastTime + timeDiff)

-- convert a Codec.Midi message into a PortMidi message
-- TODO: look more into the PMMsg form
toPMMsg :: Message -> Maybe PMMsg
toPMMsg m@(NoteOn ch pitch velocity) = Just $ PMMsg (fromIntegral $ eventStatus m ch) (fromIntegral pitch) (fromIntegral velocity)
toPMMsg m@(NoteOff ch pitch _) = Just $ PMMsg (fromIntegral $ eventStatus m ch) (fromIntegral pitch) 0
toPMMsg m@(ProgramChange ch program) = Just $ PMMsg (fromIntegral $ eventStatus m ch) (fromIntegral program) 0
toPMMsg _ = Nothing -- TempoChange don't translate directly to PortMidi, so we'll ignore them

eventStatus :: Message -> Channel -> Int
eventStatus msg ch = base msg + fromIntegral ch
    where base (NoteOn _ _ _) = 0x90
          base (NoteOff _ _ _) = 0x80
          base (ProgramChange _ _) = 0xC0

-- convert codec.midi ticks to microseconds
-- we make use of ppq = pulses per quarter note -> stored in codec.midi timeDiv
-- TODO: do i actually need milliseconds...?
ticksToMicroseconds :: Int -> Int -> Int -> Int
ticksToMicroseconds ticks tempo ppq = (ticks * tempo) `div` ppq