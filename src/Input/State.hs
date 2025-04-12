module Input.State where

import Music.Data
import Text.Parsec

-- new parser with the custom state
type MyParser = ParsecT String ParsingState IO

data ParsingState = PState {
    tracks :: [(String, TrackE)],  -- variables which were just defined
    melodies :: [(String, Music)]  -- variables which were given context & are ready to be played and saved
}

emptyState :: ParsingState
emptyState = PState [] []

addTrack :: String -> TrackE -> ParsingState -> ParsingState
addTrack name track state = state {tracks = (name, track) : tracks state}

-- find in the state the value of the identifier provided
getValue :: ParsingState -> String -> (Maybe TrackE, Maybe Music)
getValue state name = (lookup name (tracks state), lookup name (melodies state))

printValue :: ParsingState -> String -> IO ()
printValue state name = 
    let (track, music) = getValue state name
    in case track of
        Nothing -> case music of
                    Nothing -> putStrLn "No structures found with the provided name"
                    Just melody -> print melody
        Just track -> print track