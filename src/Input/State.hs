module Input.State where

import Music.Data
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
getValue :: ParsingState -> String -> (Maybe Track, Maybe Music)
getValue state name = (lookup name (tracks state), lookup name (melodies state))

printValue :: ParsingState -> String -> IO ()
printValue state name = 
    let (track, music) = getValue state name
    in case track of
        Nothing -> case music of
                    Nothing -> putStrLn "No structures found with the provided name"
                    Just melody -> print melody
        Just track -> print track
