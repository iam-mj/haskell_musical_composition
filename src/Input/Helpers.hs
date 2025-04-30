module Input.Helpers where

import Input.State
import Input.Messages
import Music.Data
import Text.Parsec
import Control.Monad.IO.Class (liftIO)
import Music.Utils
import MIDI.Synthesizer (playMidiFile)
import MIDI.ToMIDI (playMusic, saveMusic)
import Data.Maybe (fromJust)


------------------------------------------------
--                  TYPES                     --
------------------------------------------------

type Index   = Int
type Indexes = (Index, Maybe Index) -- one index (e.g. 1) or the limits of an interval of indexes (e.g. 2-4) 

type IndexOrError   = Either Error Index
type IndexesOrError = Either Error Indexes

type Repeat          = Int
type RepeatNote      = (Primitive, Repeat)
type RepeatDuo       = (Int, Primitive, Repeat)
type RepeatChord     = (Chord, Primitive, Repeat)
type PitchWithChange = (Pitch, OctaveChange)



------------------------------------------------
--               FUNCTIONAL                   --
------------------------------------------------


-- parse a string from an assciation list and return it's associated value
mapString :: [(String, a)] -> MyParser a
mapString list = do
    key <- choice (map ((try . string) . fst) list)
    return $ fromJust $ lookup key list

liftPutStrLn :: String -> MyParser ()
liftPutStrLn = liftIO . putStrLn

tryAddTrack :: Name -> Track -> MyParser ()
tryAddTrack name track = do
    state <- getState
    let newState = addTrack name track state
    case newState of
        Left err    -> do
            let Just message = lookup TrackAddFail logs
            liftPutStrLn err
            liftPutStrLn $ message name
        Right newSt -> do
            let Just message = lookup TrackAddSuccess logs
            putState newSt
            liftPutStrLn $ message name

tryAddMusic :: Name -> Name -> Int -> Instrument -> MyParser ()
tryAddMusic name musicName oct instrument = do
    -- get the track from the current state and create the music with the given context
    state <- getState
    let gotTrack = getTrack state name
    case gotTrack of
        Left err    -> liftPutStrLn err
        Right track -> do
            let music       = Music (interpret track) oct instrument
            -- try to add the new melody to the state
            let newState = addMusic musicName music state
            case newState of
                Left err    -> do 
                    let Just message = lookup MelodyAddFail logs
                    liftPutStrLn err
                    liftPutStrLn $ message musicName
                Right newSt -> do
                    let Just message = lookup MelodyAddSuccess logs
                    putState newSt
                    liftPutStrLn $ message musicName

showAllHelper :: MyParser ()
showAllHelper = do
    state <- getState
    liftIO $ printState state

showOneHelper :: Name -> MyParser ()
showOneHelper name = do
    state <- getState
    liftIO $ printValue state name

tryPlayFile :: String -> MyParser ()
tryPlayFile fileName = do
    let validationErr = validatePath fileName
    case validationErr of
        Nothing  -> liftIO $ playMidiFile fileName
        Just err -> liftPutStrLn err

tryPlayValue :: Name -> MyParser ()
tryPlayValue name = do
    state <- getState
    let gotMusic = getMusic state name
    case gotMusic of
        Left err    -> liftPutStrLn err
        Right music -> liftIO $ playMusic music

saveToFile :: Name -> String -> MyParser ()
saveToFile name fileName = do
    let validationErr = validatePath fileName
    case validationErr of
        Nothing -> do
            state <- getState
            let gotMusic = getMusic state name
            case gotMusic of
                Left err    -> liftPutStrLn err
                Right music -> do
                    let Just message = lookup FileSave logs
                    liftIO $ saveMusic music fileName
                    liftPutStrLn $ message fileName
        Just err -> liftPutStrLn err

callInsert :: Name -> IndexOrError -> Name -> MyParser ()
callInsert name idx insertName = do
    case idx of
        Left err  -> liftPutStrLn err
        Right idx -> do
            state <- getState
            let gotInsert = getTrack state insertName
                gotTrack  = getTrack state name
            case gotInsert of
                Left err     -> liftPutStrLn err
                Right insert -> do
                    case gotTrack of
                        Left err    -> liftPutStrLn err
                        Right track -> do
                            let newTrack     = insertT track idx insert
                                Just message = lookup TrackModifySuccess logs
                            modifyState $ updateTrack name newTrack
                            liftPutStrLn $ message name
                            liftIO $ print newTrack

callDelete :: Name -> IndexesOrError -> MyParser ()
callDelete name indexes = do
    case indexes of
        Left err   -> liftPutStrLn err
        Right idxs -> do
            state <- getState
            let gotTrack = getTrack state name
            case gotTrack of
                Left err    -> liftPutStrLn err
                Right track -> do
                    let newTrack     = deleteT track idxs
                        Just message = lookup TrackModifySuccess logs
                    modifyState $ updateTrack name newTrack
                    liftPutStrLn $ message name
                    liftIO $ print newTrack

callReplace :: Name -> IndexesOrError -> String -> MyParser ()
callReplace name indexes replaceName = do
    case indexes of
        Left err   -> liftPutStrLn err
        Right idxs -> do
            state <- getState
            let gotTrack     = getTrack state name
                gotReplaceTr = getTrack state replaceName
            case gotTrack of
                Left err    -> liftPutStrLn err
                Right track -> do
                    case gotReplaceTr of
                        Left err        -> liftPutStrLn err
                        Right replaceTr -> do
                            let newTrack     = replaceT track idxs replaceTr
                                Just message = lookup TrackModifySuccess logs
                            modifyState $ updateTrack name newTrack
                            liftPutStrLn $ message name
                            liftIO $ print newTrack

callParallelize :: Name -> Name -> MyParser ()
callParallelize name paraName = do
    state <- getState
    let gotMusic     = getMusic state name
        gotParaMusic = getMusic state paraName
    case gotMusic of
        Left err    -> liftPutStrLn err
        Right music -> do 
            case gotParaMusic of
                Left err        -> liftPutStrLn err
                Right paraMusic -> do
                    let newMusic     = music ::: paraMusic
                        Just message = lookup MelodyModifySuccess logs
                    modifyState $ updateMusic name newMusic
                    liftPutStrLn $ message name
                    liftIO $ print newMusic

callSequence :: Name -> Maybe Int -> Name -> MyParser ()
callSequence name num seqName = do
    state <- getState
    let gotTrack = getTrack state name
        gotSeqTr = getTrack state seqName
    case gotTrack of
        Left err    -> liftPutStrLn err
        Right track -> do
            case gotSeqTr of
                Left err    -> liftPutStrLn err
                Right seqTr -> do
                    let newTrack     = addRepeatT track seqTr num
                        Just message = lookup TrackModifySuccess logs
                    modifyState $ updateTrack name newTrack
                    liftPutStrLn $ message name
                    liftIO $ print newTrack

callTranspose :: Name -> Int -> MyParser ()
callTranspose name num = do
    state <- getState
    let value    = getValue state name
        newValue = transposeValue value num
    case newValue of
        Left err        -> liftPutStrLn err
        Right structure -> case structure of
            Left track  -> do
                let Just message = lookup TrackTransSuccess logs
                modifyState $ updateTrack name track
                liftPutStrLn $ message name
                liftIO $ print track
            Right music -> do
                let Just message = lookup MelodyTransSuccess logs
                modifyState $ updateMusic name music 
                liftPutStrLn $ message name
                liftIO $ print music

callClean :: Name -> MyParser ()
callClean name = do
    state <- getState
    let gotTrack = getTrack state name
    case gotTrack of
        Left err    -> liftPutStrLn err
        Right track -> do
            let newTrack     = cleanT track
                Just message = lookup TrackModifySuccess logs
            modifyState $ updateTrack name newTrack
            liftPutStrLn $ message name
            liftIO $ print newTrack



------------------------------------------------
--             TRANSFORMATIONS                --
------------------------------------------------


groupsToTrack :: [[Group]] -> MyParser Track
groupsToTrack = return . link . concat

repeatList :: Int -> [a] -> [a]
repeatList n = concat . replicate n

notesToGroups :: [RepeatNote] -> Maybe Repeat -> MyParser [Group]
notesToGroups repeatNotes repLine = do
    -- transform the (note, int) tuples into groups
    let repeatNote (note, n) = replicate n note
        notes                = concatMap repeatNote repeatNotes
        groups               = map Single notes
    -- repeat the line if necessary
    case repLine of
        Nothing     -> return groups
        Just repeat -> return $ repeatList repeat groups

restToGroups :: Duration -> Maybe Repeat -> MyParser [Group]
restToGroups dur repLine = do
    let groups = [(Single . Rest) dur]
    case repLine of
        Nothing     -> return groups
        Just repeat -> return $ repeatList repeat groups

duosToGroups :: [RepeatDuo] -> Maybe Repeat -> MyParser [Group]
duosToGroups repeatDuos repLine = do
    -- transform (int, note, rep) tuples into groups
    let repeatDuo (int, note, rep) = replicate rep (int, note)
        duos                       = concatMap repeatDuo repeatDuos
        groups                     = map (uncurry Duo) duos
    -- repeat the line of duos if necessary
    case repLine of
        Nothing     -> return groups
        Just repeat -> return $ repeatList repeat groups

chordsToGroups :: [RepeatChord] -> Maybe Repeat -> MyParser [Group]
chordsToGroups repeatChords repLine = do
    -- transform (chord, note, rep) tuples to groups 
    let repeatChord (chd, note, rep) = replicate rep (chd, note)
        chords                       = concatMap repeatChord repeatChords
        groups                       = map (uncurry Chord) chords
    -- repeat the line of chords if necessary
    case repLine of
        Nothing     -> return groups
        Just repeat -> return $ repeatList repeat groups



------------------------------------------------
--                 MAKERS                     --
------------------------------------------------

makePitchWithChange :: Pitch -> Maybe OctaveChange -> MyParser PitchWithChange
makePitchWithChange pitch ch = do
    case ch of
        Nothing -> return (pitch, noChange)
        Just ch -> return (pitch, fromIntegral ch)

makeRepeatNote :: Pitch -> Duration -> OctaveChange -> Maybe Repeat -> MyParser RepeatNote
makeRepeatNote pitch dur ch rep = do
    case rep of
        Nothing     -> return (Note pitch dur ch, 1)
        Just repeat -> return (Note pitch dur ch, repeat)

makeIndex :: Index -> MyParser IndexOrError
makeIndex idx = do
    let err = checkIndex idx 
    case err of
        Nothing    -> return $ Right (idx - 1)
        Just error -> return $ Left error

-- make indexes or error when 2 limits for an index interval should be provided
makeIndexes2 :: IndexOrError -> IndexOrError -> MyParser IndexesOrError
makeIndexes2 left right = do
    case left of
        Left err      -> return $ Left err
        Right leftIdx -> do
            case right of
                Left err       -> return $ Left err
                Right rightIdx -> return $ Right (leftIdx, Just rightIdx)

-- make indexes or error when we only have one index and not an interval
makeIndexes1 :: IndexOrError -> MyParser IndexesOrError
makeIndexes1 idx = do
    case idx of
        Left err    -> return $ Left err
        Right index -> return $ Right (index, Nothing)