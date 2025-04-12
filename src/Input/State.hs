module Input.State where

import Music.Data
import Music.Utils
import Text.Parsec

-- new parser with the custom state
type MyParser = ParsecT String ParsingState IO

data ParsingState = PState {
    tracks :: [(String, Track)],  -- variables which were just defined
    melodies :: [(String, Music)]  -- variables which were given context & are ready to be played and saved
}

emptyState :: ParsingState
emptyState = PState [] []

addTrack :: String -> Track -> ParsingState -> ParsingState
addTrack name track state = state {tracks = (name, track) : tracks state}

addMusic :: String -> Music -> ParsingState -> ParsingState
addMusic name music state = state {melodies = (name, music) : melodies state}

getTrack :: ParsingState -> String -> Either (IO ()) Track
getTrack state name = case lookup name (tracks state) of
                        Nothing    -> Left $ error "No tracks found with the given name!"
                        Just track -> Right track

getMusic :: ParsingState -> String -> Either (IO ()) Music
getMusic state name = case lookup name (melodies state) of
                        Nothing    -> Left $ error "No melodies found with the given name!"
                        Just music -> Right music

-- find in the state the value of the identifier provided
getValue :: ParsingState -> String -> Maybe (Either Track Music)
getValue state name = case lookup name (tracks state) of
                        Nothing     -> case lookup name (melodies state) of
                                            Nothing    -> Nothing
                                            Just music -> Just $ Right music
                        Just track  -> Just $ Left track

updateList :: String -> a -> [(String, a)] -> [(String, a)]
updateList name newValue [] = []
updateList name newValue ((name', value) : rest)
    | name' == name = (name', newValue) : rest
    | otherwise     = (name', value) : updateList name newValue rest

-- replace a track with a new value
updateTrack :: String -> Track -> ParsingState -> ParsingState
updateTrack name track state = state {tracks = updateList name track (tracks state)}

-- replace a melody with a new value
updateMusic :: String -> Music -> ParsingState -> ParsingState
updateMusic name music state = state {melodies = updateList name music (melodies state)}

printValue :: ParsingState -> String -> IO ()
printValue state name =
    let value = getValue state name
    in case value of
        Nothing        -> putStrLn "No structures found with the provided name"
        Just structure -> case structure of
                            Left track  -> print track
                            Right music -> print music

-- transpose either a track or a music with a number of semitones
transposeValue :: Maybe (Either Track Music) -> Int -> Either (IO ()) (Either Track Music)
transposeValue value num = 
    case value of 
        Nothing        -> Left $ putStrLn "No structures found with the provided name"
        Just structure -> case structure of
                            Left track  -> Right $ Left $ transposeT track num
                            Right music -> Right $ Right $ transposeM music num 
