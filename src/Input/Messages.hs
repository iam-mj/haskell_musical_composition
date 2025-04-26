module Input.Messages where

---------------------------------------------
--               ERRORS                    --
---------------------------------------------

type Error = String

data ErrorKey = NotUniqueName | NoTrackName | NoMelodyName | NoName | NegativeIndex | NotMidiFile
                deriving Eq

errorMessages :: [(ErrorKey, String -> Error)]
errorMessages = [
    (NotUniqueName, \name -> "Error: There is already a record of the name " ++ name ++ ". Please enter a unique name."),
    (NoTrackName, ("Error: No tracks found with the name " ++)),
    (NoMelodyName, ("Error: No melodies found with the name " ++)),
    (NoName, ("Error: No structures found with the name " ++)),
    (NegativeIndex, ("Error: The index must have a positive value, unlike the provided " ++)),
    (NotMidiFile, ("Error: Please provide a file path to a MIDI file - it should have a '.mid' extension, not " ++))]


---------------------------------------------
--             INFO LOGS                   --
---------------------------------------------

data LogKey = TrackAddFail | TrackAddSuccess    | MelodyAddSuccess    | MelodyAddFail     |
              FileSave     | TrackModifySuccess | MelodyModifySuccess | TrackTransSuccess |
              MelodyTransSuccess
              deriving Eq

logs :: [(LogKey, String -> String)]
logs = [(TrackAddFail, ("Failed to add track " ++)),
        (TrackAddSuccess, \name -> "Track " ++ name ++ " added succesfully"),
        (MelodyAddFail, ("Failed to add melody " ++)),
        (MelodyAddSuccess, \name -> "Melody " ++ name ++ " added succesfully"),
        (FileSave, \name -> "File " ++ name ++ " saved succesfully"),
        (TrackModifySuccess, \name -> "Track " ++ name ++ " modified succesfully"),
        (MelodyModifySuccess, \name -> "Melody " ++ name ++ " modified succesfully"),
        (TrackTransSuccess, \name -> "Track " ++ name ++ " was transposed succesfully"),
        (MelodyTransSuccess, \name -> "Melody " ++ name ++ " was transposed succesfully")]