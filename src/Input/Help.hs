module Input.Help where

import Input.State (MyParser, Name)
import Input.Fundamental
import Input.Helpers (log)
import Prelude hiding (log)
import Text.Parsec
import Input.Mappings (stringToInstrument, stringToPitch, stringToInterval, stringToChord)

-------------------------------
--           UTILS           --
-------------------------------

-- format a list of strings as a single string representing an unordered list
formatList :: [String] -> String
formatList = foldl (\str instr -> str ++ " - " ++ instr ++ "\n") ""

formatMapping :: Show a => [(String, a)] -> String
formatMapping = foldl (\str (name, value) -> str ++ " - " ++ name ++ " (i.e. " ++ show value ++ ")\n") ""

-------------------------------
--          PARSERS          --
-------------------------------

---------- GENERAL ------------

generalHelp :: MyParser ()
generalHelp = eol >> printGeneral

printGeneral :: MyParser ()
printGeneral = do
    log "\nWelcome to Prelude!"
    log "Here are the available instructions. Try typing \"help [instruction-name]\" to get more information on specific instructions."
    log $ foldl (\str (name, instrs) -> str ++ "\n" ++ name ++ "\n" ++ formatList instrs) "" instructions
    log "Type \"exit\" to stop the program.\n"

instructions :: [(Name, [String])]
instructions = [("Music Composition",   ["track", "melody", "modify"]),
                ("Music Management",    ["read", "show", "play", "save", "load"]),
                ("Music Visualization", ["vis", "score", "see"])]

---------- INSTRUCTIONS -----------

instructionHelp :: MyParser ()
instructionHelp = choice $ map try $ defineInstr ++ manageInstr ++ visualInstr
    where defineInstr = [trackHelp,  pitchHelp, durationHelp, intervalHelp, chordHelp, melodyHelp, modifyHelp]
          manageInstr = [showHelp, readHelp, playHelp, saveHelp, loadHelp]
          visualInstr = [visHelp, scoreHelp, seeHelp]

-------------- TRACK --------------

trackHelp :: MyParser ()
trackHelp = string "track" >> printTrack
    where printTrack = do
            log "\nTo create and record a Track with a certain name, we use the \"track\" instruction:\n"
            log "\ttrack [name] {"
            log "\t    GROUPS"
            log "\t}"
            log "\nTrack names should be unique and only contain letters, digits and underscores (\"_\")."
            log "\nGROUPS are groups of notes played at the same time and can use the following constructors:"
            log "\tnote:  [pitch] [duration]                          -- one note"
            log "\trest:  [duration]                                  -- one rest"
            log "\tduo:   [interval] [root-pitch] [root-duration]     -- two notes"
            log "\tchord: [chord] [root-pitch] [root-duration]        -- three or more notes"
            log "\nThe groups will be played from top to bottom, and if multiple groups of the same type are needed one after the other, they can be sequenced using a comma, e.g:"
            log "\tnote: A en, B hn, C qn"
            log "\nGroups can also be repeated: either only one, or a whole line of groups:"
            log "\tnote: A en x[number], B hn                         -- repeat only the group (note A en)"
            log "\tnote: A en, B hn (x[number])                       -- repeat the line of groups (note A en, note B hn)"
            log "\nFor more information regarding specific parts of groups, see also:"
            log $ formatList $ map ("help " ++ ) ["pitch", "duration", "interval", "chord"]

-------------- PITCH --------------

pitchHelp :: MyParser ()
pitchHelp = string "pitch" >> printPitch
    where printPitch = do
            log "\nWhen defining a note, we accept the following pitch classes, in accordance with the international note notation:"
            log "\tC, D, E, F, G, A, B"
            log "\nWe accept the following note accessories:"
            log " - # and b (sharp and flat), e.g.: A#, Bb"
            log " - octave change (relative to the reference octave given in a melody's context), e.g.: C(+1), D(-2)\n"

-------------- DURATION ------------

durationHelp :: MyParser ()
durationHelp = string "duration" >> printDuration
    where printDuration = do
            log "\nWhen defining a note or a rest, we accept the following notations for duration:"
            log $ formatMapping durations
            log "We also accept the addition of a dot (\".\") to any duration notation, with the expected effect of extending the note by 1/2 of its duration.\n"

durations :: [(String, String)]
durations = [("bn", "breve, double whole note"), ("wn", "whole note"), ("hn", "half note"), ("qn", "quarter note"),
             ("en", "eighth note"), ("sn", "sixteenth note"), ("tn", "thiry-second note"), ("sfn", "sixty-fourth note")]

-------------- INTERVAL --------------

intervalHelp :: MyParser ()
intervalHelp = string "interval" >> printInterval
    where printInterval = do
            log "\nWhen defining a duo group, the interval parameter gives the number of semitones between the root note, and the other note."
            log "Therefore, we accept either an integer with the aforementioned meaning, or one of the following:"
            log $ formatMapping stringToInterval

--------------- CHORD ---------------

chordHelp :: MyParser ()
chordHelp = string "chord" >> printChord
    where printChord = do
            log "\nWhen defining a chord group, the chord parameter sets the chord type by providing the number of semitones between the root note and each other note in the chord."
            log "\nThere are 2 different ways of setting a chord type:"
            log "\n1) Using one of the following default notations:"
            log $ formatMapping stringToChord
            log "2) Defining a custom chord:"
            log "\t<semitones>"
            log "The semitones should be natural numbers separated by a comma and the first one should always be 0 (for the difference between the root note and itself).\n"

-------------- MELODY --------------

melodyHelp :: MyParser ()
melodyHelp = string "melody" >> printMelody
    where printMelody = do
            log "\nTo create and record a Melody with a certain name, we use the \"melody\" instruction:\n"
            log "\tmelody [melody-name] { [track-name] oct[octave-number] [instrument] }"
            log "\nMelody names should be unique and only contain letters, digits and underscores (\"_\")."
            log "Melodies are Tracks bound with a context, therefore the [track-name] provided must be the name of a valid recorded track."
            log "Hint: check the recorded tracks with the \"show\" instructions."
            log "\nThe octave number is expected to be an integer between -1 and 9, in accordance with the Standard MIDI numbers."
            log "The instrument name is expected to be part of the General MIDI Standard, or one of the following:"
            log $ formatMapping stringToInstrument

-------------- SHOW --------------

showHelp :: MyParser ()
showHelp = string "show" >> printShow
    where printShow = do
            log "\nTo show a certain Track or Melody, we use the \"show\" instruction:"
            log "\tshow [name]"
            log "\nFor printing the names of all the recorded Tracks and Melodies, we can use a variation of \"show\":"
            log "\tshow -all\n"

-------------- READ --------------

readHelp :: MyParser ()
readHelp = string "read" >> printRead
    where printRead = do
            log "\nTo read a \".txt\" file written in Prelude, we can use the \"read\" instruction:"
            log "\tread [path-to-file]"
            log "\nThe path to the source file should be enclosed between quotes (\"\")."
            log "\nPlease note that in Prelude, instructions must be separated by an empty line.\n"

-------------- PLAY --------------

playHelp :: MyParser ()
playHelp = string "play" >> printPlay
    where printPlay = do
            log "\nTo play a recorded Melody, we use the \"play\" instruction:"
            log "\tplay [melody-name]"
            log "\nWe can also play a \".mid\" (Standard MIDI) file:"
            log "\tplay [path-to-file]"
            log "The path must be enclosed between quotes (\"\")."
            log "\nWe can also play a Track by giving it the Melody context, but without recording a new Melody, like so:"
            log "\tplay { [track-name] oct[octave-number] [instrument]}"
            log "For details on the Melody context, see also \"help melody\".\n"

-------------- SAVE --------------

saveHelp :: MyParser ()
saveHelp = string "save" >> printSave
    where printSave = do
            log "\nTo save a Melody to a \".mid\" (Standard MIDI) file, we use the \"save\" instruction:"
            log "\tsave [melody-name] [path-to-new-MIDI-file]"
            log "\nThe path to the MIDI file which will be created should be enclosed in quotes (\"\"). If the file already exists, it will be overwritten.\n"

-------------- LOAD --------------

loadHelp :: MyParser ()
loadHelp = string "load" >> printLoad
    where printLoad = do
            log "\nTo load a \".mid\" (Standard MIDI) file as a Melody in Prelude, we use the \"load\" instruction:"
            log "\tload [path-to-file] [new-melody-name]"
            log "\nThe path to the MIDI file should be enclosed in quotes (\"\")."
            log "The new Melody name should be unique and only contain letters, digits and the underscores (\"_\").\n"

-------------- VIS --------------

visHelp :: MyParser ()
visHelp = string "vis" >> printVis
    where printVis = do
            log "\nTo open an external visualization software for a Melody, we use the \"vis\" instruction:"
            log "\tvis [melody-name]"
            log "\nWe can also open the visualization software for a Melody saved in a MIDI file:"
            log "\tvis [path-to-file]"
            log "The path to the MIDI file should be enclosed in quotes (\"\")."
            log "\nCurrently, the \"vis\" instruction will open up a \"Noteflight\" score importing the Melody in the browser."
            log "Please take note of the fact that \"Noteflight\" confirmed an existing bug at MIDI files import, which makes the created score representation unpredictable."
            log "\nPrelude uses the Noteflight Launch API. For more information about the API, please visit the Noteflight documentation at https://www.noteflight.com/doc/api/launch."
            log "\nNoteflight is a registered trademark of Hal Leonard, LLC. Prelude uses the Noteflight Launch API under the terms outlined in their documentation. All rights to Noteflight and its services remain with Hal Leonard, LLC.\n"

-------------- SCORE --------------

scoreHelp :: MyParser ()
scoreHelp = string "score" >> printScore
    where printScore = do
            log "\nTo open a \"html-midi-player\" visualization for a Melody, we use the \"score\" instruction:"
            log "\tscore [melody-name]"
            log "\nWe can also open the visualization for a Melody saved in a MIDI file:"
            log "\tscore [path-to-file]"
            log "\nThe path to the MIDI file should be enclosed in quotes (\"\")."
            log "\nThe \"html-midi-player\" visualization will depict the score representation of the Melody, while also providing audio rendering of the Melody."
            log "The \"html-midi-player\" visualization only works as expected for Melodies with a single instrument."
            log "\nPrelude uses html-midi-player (https://github.com/cifkao/html-midi-player), a set of web components developed by @cifkao(https://github.com/cifkao), to play MIDI files directly in the browser."
            log "\nhtml-midi-player is used under the terms of the MIT License. This project is not affiliated with or endorsed by Cifkao.\n"

-------------- SEE --------------

seeHelp :: MyParser ()
seeHelp = string "see" >> printSee
    where printSee = do
            log "\nTo open a Prelude-specific visualization for a Track, we use the \"see\" instruction. We also need to provide a Melody context for the Track in order to be able to play it along with the visualization."
            log "\tsee { [track-name] oct[octave-number] [instrument] }"
            log "\nFor details on the Melody context, see also \"help melody\"."
            log "\nThe visualization will offer an alternate way of visualizing a Melody, different from the classic score representation. It brings focus to the groups which make up a Track and the connection between them.\n"

-------------- MODIFY --------------

modifyHelp :: MyParser ()
modifyHelp = string "modify" >> printModify
    where printModify = do
            log "\nThere are many operations available through the \"modify\" instruction:"
            log "\n1) Index operations on Tracks:"
            log "\tmodify [track-name] insert [index] [inserted-track-name]"
            log "\tmodify [track-name] delete [indexes]"
            log "\tmodify [track-name] replace [indexes] [replacing-track-name]"
            log "\nThe indexes are shown when calling the \"show\" command on a Track."
            log "Delete or replace a range of indexes by using a hyphen (\"-\"), e.g.: 2-5."
            log "\n2) Parallelize 2 Melodies:"
            log "\tmodify [melody-name] || [second-melody-name]"
            log "\nNecessary operation in order to obtain Melodies with multiple instruments and in different octaves."
            log "\nThe result will be kept in the first Melody."
            log "\n3) Sequence 2 Tracks:"
            log "\tmodify [track-name] ++ [second-track-name]"
            log "\nWe can also sequence the second track multiple times:"
            log "\tmodify [track-name] ++[repeat-number] [second-track-name]"
            log "\n The result will be kept in the first Track."
            log "\n4) Transpose a Melody or a Track with a number of semitones:"
            log "\tmodify [name] transpose [number-of-semitones]"
            log "\n5) Flatten certain pitches in a Melody or a Track:"
            log "\tmodify [name] flatten [pitches]"
            log "\nNecessary operation to get to minor scales."
            log "\nThe pitches should be separated by a comma, e.g.: A#, Bb.\n"