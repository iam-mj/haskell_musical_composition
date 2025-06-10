module Input.Messages where

---------------------------------------------
--               ERRORS                    --
---------------------------------------------

type Error = String

data ErrorKey = NotUniqueName  | NoTrackName | NoMelodyName | NoName | NegativeIndex | NotMidiFile | NoFile
              | NotSingleTrack | NoNegative
                deriving Eq

errorMessages :: [(ErrorKey, String -> Error)]
errorMessages = [
    (NotUniqueName,  \name -> "Error: There is already a record of the name " ++ name ++ ". Please enter a unique name."),
    (NoTrackName,    ("Error: No tracks found with the name " ++)),
    (NoMelodyName,   ("Error: No melodies found with the name " ++)),
    (NoName,         ("Error: No structures found with the name " ++)),
    (NegativeIndex,  ("Error: The index must have a positive value, unlike the provided " ++)),
    (NotMidiFile,    ("Error: Please provide a file path to a MIDI file - it should have a '.mid' extension, not " ++)),
    (NoFile,         \name -> "Error: The provided file " ++ name ++ " cannot be found"),
    (NotSingleTrack, \name -> "Error: The provided music " ++ name ++ " has more than one instrument. Cannot visualize it."),
    (NoNegative,     ("Error: Expected positive value instead of " ++))]


---------------------------------------------
--             INFO LOGS                   --
---------------------------------------------

data LogKey = TrackAddFail   | TrackAddSuccess    | MelodyAddSuccess    | MelodyAddFail        |
              FileSave       | TrackModifySuccess | MelodyModifySuccess | TrackTransSuccess    |
              MelodyLoadFail | MelodyTransSuccess | TrackFlattenSuccess | MelodyFlattenSuccess |
              ReadRollBack   
              deriving Eq

logs :: [(LogKey, String -> String)]
logs = [(TrackAddFail,         ("Failed to add track " ++)),
        (TrackAddSuccess,      \name -> "Track " ++ name ++ " added succesfully"),
        (MelodyAddFail,        ("Failed to add melody " ++)),
        (MelodyAddSuccess,     \name -> "Melody " ++ name ++ " added succesfully"),
        (FileSave,             \name -> "File " ++ name ++ " saved succesfully"),
        (TrackModifySuccess,   \name -> "Track " ++ name ++ " modified succesfully"),
        (MelodyModifySuccess,  \name -> "Melody " ++ name ++ " modified succesfully"),
        (TrackTransSuccess,    \name -> "Track " ++ name ++ " was transposed succesfully"),
        (MelodyTransSuccess,   \name -> "Melody " ++ name ++ " was transposed succesfully"),
        (MelodyLoadFail,       ("Failed to load melody from file " ++)),
        (TrackFlattenSuccess,  \name -> "Track " ++ name ++ " was flattened successfully"),
        (MelodyFlattenSuccess, \name -> "Melody " ++ name ++ " was flattened successfully"),
        (ReadRollBack,         \name -> "Reading of file " ++ name ++ " was unsuccessful. All file operations were ROLLED BACK")]