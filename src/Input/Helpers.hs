{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
module Input.Helpers where

import Input.State
import Input.Messages
import MIDI.Synthesizer (playMidiFile)
import MIDI.ToMIDI (playMusic, saveMusic)
import MIDI.FromMIDI (loadMusic)
import Music.Data
import Music.Utils
import Visual.CreateScore

import Text.Parsec
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Prelude hiding (log)
import System.Directory (removeFile, makeRelativeToCurrentDirectory, createDirectory, removeDirectory)
import GHC.Conc.IO (threadDelay)


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

log :: String -> MyParser ()
log = liftIO . putStrLn

getTrackAnd :: Name -> (Track -> MyParser ()) -> MyParser ()
getTrackAnd name parser = do
    state <- getState
    let gotTrack = getTrack state name
    case gotTrack of 
        Left err    -> log err
        Right track -> parser track

getMusicAnd :: Name -> (Music -> MyParser ()) -> MyParser ()
getMusicAnd name parser = do
    state <- getState
    let gotMusic = getMusic state name
    case gotMusic of
        Left err    -> log err
        Right music -> parser music

tryAddTrack :: Name -> Track -> MyParser ()
tryAddTrack name track = do
    state <- getState
    let newState = addTrack name track state
    case newState of
        Left err    -> do
            let Just message = lookup TrackAddFail logs
            log err
            log $ message name
        Right newSt -> do
            let Just message = lookup TrackAddSuccess logs
            putState newSt
            log $ message name

tryAddMusic :: Name -> Name -> Int -> Instrument -> MyParser ()
tryAddMusic name musicName oct instrument = getTrackAnd name createMusic
    where createMusic track = do
            state <- getState
            let music = Music (interpret track) oct instrument
            callAddMusic musicName music state
            
callAddMusic :: Name -> Music -> ParsingState -> MyParser ()
callAddMusic name music state = do
    let newState = addMusic name music state
    case newState of
        Left err    -> do 
            let Just message = lookup MelodyAddFail logs
            log err
            log $ message name
        Right newSt -> do
            let Just message = lookup MelodyAddSuccess logs
            putState newSt
            log $ message name

showAllHelper :: MyParser ()
showAllHelper = do
    state <- getState
    liftIO $ printState state

showOneHelper :: Name -> MyParser ()
showOneHelper name = do
    state <- getState
    liftIO $ printValue state name

tryPlayFile :: String -> MyParser ()
tryPlayFile fileName = validatePathAnd fileName (liftIO $ playMidiFile fileName)

tryPlayValue :: Name -> MyParser ()
tryPlayValue name = getMusicAnd name (liftIO . playMusic)

validatePathAnd :: String -> MyParser () -> MyParser ()
validatePathAnd fileName parser = do
    let validationErr = validatePath fileName
    case validationErr of
        Nothing  -> parser
        Just err -> log err

saveToFile :: Name -> String -> MyParser ()
saveToFile name fileName = validatePathAnd fileName saveFile 
    where saveFile       = getMusicAnd name makeSave
          makeSave music = do
                let Just message = lookup FileSave logs
                liftIO $ saveMusic music fileName
                log $ message fileName

loadFromFile :: Name -> String -> MyParser ()
loadFromFile name fileName = validatePathAnd fileName loadFile 
    where loadFile = do
            state  <- getState
            loaded <- liftIO $ loadMusic fileName
            let Just loadFail = lookup MelodyLoadFail logs
            case loaded of
                Nothing    -> log $ loadFail fileName
                Just music -> callAddMusic name music state

createScoreFromFile :: String -> MyParser ()
createScoreFromFile fileName = validatePathAnd fileName launchScore
    where launchScore = liftIO $ createScore fileName
    
defFileName name = "resources/temp_" ++ name ++ ".mid"

createScoreFromMusic :: Name -> MyParser ()
createScoreFromMusic name = getMusicAnd name launchScore
    where launchScore music = do
            let fileName = defFileName name
            liftIO $ saveMusic music fileName
            liftIO $ createScore fileName
            liftIO $ removeFile fileName

modifyTrack :: Name -> Track -> (String -> String) -> MyParser ()
modifyTrack name newTrack message = do
    modifyState $ updateTrack name newTrack
    log $ message name
    liftIO $ print newTrack

modifyMusic :: Name -> Music -> (String -> String) -> MyParser ()
modifyMusic name newMusic message = do
    modifyState $ updateMusic name newMusic
    log $ message name
    liftIO $ print newMusic

callInsert :: Name -> IndexOrError -> Name -> MyParser ()
callInsert name idx insertName = do
    case idx of
        Left err  -> log err
        Right idx -> do
            getTrackAnd insertName getTrackWithName
            where getTrackWithName insertTrack    = getTrackAnd name (makeInsertion insertTrack)
                  makeInsertion insertTrack track = do
                    let newTrack     = insertT track idx insertTrack
                        Just message = lookup TrackModifySuccess logs
                    modifyTrack name newTrack message

callDelete :: Name -> IndexesOrError -> MyParser ()
callDelete name indexes = do
    case indexes of
        Left err   -> log err
        Right idxs -> do
            getTrackAnd name deleteTrack
            where deleteTrack track = do
                    let newTrack     = deleteT track idxs
                        Just message = lookup TrackModifySuccess logs
                    modifyTrack name newTrack message

callReplace :: Name -> IndexesOrError -> String -> MyParser ()
callReplace name indexes replaceName = do
    case indexes of
        Left err   -> log err
        Right idxs -> getTrackAnd name getReplaceTrack
            where getReplaceTrack track           = getTrackAnd replaceName (makeReplacement track)
                  makeReplacement track replaceTr = do
                    let newTrack     = replaceT track idxs replaceTr
                        Just message = lookup TrackModifySuccess logs
                    modifyTrack name newTrack message

callParallelize :: Name -> Name -> MyParser ()
callParallelize name paraName = getMusicAnd name getParaName
    where getParaName music        = getMusicAnd paraName (makePara music)
          makePara music paraMusic = do
            let newMusic     = music ::: paraMusic
                Just message = lookup MelodyModifySuccess logs
            modifyMusic name newMusic message

callSequence :: Name -> Maybe Int -> Name -> MyParser ()
callSequence name num seqName = getTrackAnd name getSeqTrack
    where getSeqTrack track   = getTrackAnd seqName (makeSeq track)
          makeSeq track seqTr = do
            let newTrack     = addRepeatT track seqTr num
                Just message = lookup TrackModifySuccess logs
            modifyTrack name newTrack message

callTranspose :: Name -> Int -> MyParser ()
callTranspose name num = do
    state <- getState
    let value    = getValue state name
        newValue = transposeValue value num
    case newValue of
        Left err        -> log err
        Right structure -> case structure of
            Left track  -> do
                let Just message = lookup TrackTransSuccess logs
                modifyTrack name track message
            Right music -> do
                let Just message = lookup MelodyTransSuccess logs
                modifyMusic name music message

callClean :: Name -> MyParser ()
callClean name = getTrackAnd name cleanTr 
    where cleanTr track = do
            let newTrack     = cleanT track
                Just message = lookup TrackModifySuccess logs
            modifyTrack name newTrack message



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