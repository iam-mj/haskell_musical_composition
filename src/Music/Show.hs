module Music.Show where
import Music.Data
import Music.Utils

instance Show Track where
    show track = showIndex track 1
        where
            showIndex EmptyT n = "[" ++ show n ++ "]"
            showIndex (Prim (Single (Rest dur))) n = "rest: " ++ show dur ++ showInd n
            showIndex (Prim (Single note)) n       = "note: " ++ showNote note ++ showInd n
            showIndex (Prim (Duo int note)) n      = "duo: " ++ show int ++ " " ++ showNote note ++ showInd n
            showIndex (Prim (Chord chord note)) n  = "chord: " ++ show chord ++ " " ++ showNote note ++ showInd n
            showIndex (track1 :+: track2) n        = showIndex track1 n ++ ", " ++ showIndex track2 (n + lengthTr1)
                where lengthTr1 = lengthT track1 

showNote (Note ptch dur octCh)
    | octCh == 0 = show ptch ++ " " ++ show dur
    | octCh > 0  = show ptch ++ "(+" ++ show octCh ++ ") " ++ show dur
    | octCh < 0  = show ptch ++ "("  ++ show octCh ++ ") " ++ show dur

showInd n = " [" ++ show n ++ "]"


instance Show TrackE where
    show EmptyET = "()"
    show (PrimET (Rest dur))     = "rest: " ++ show dur
    show (PrimET note@(Note {})) = "note: " ++ showNote note
    show (track1 :++: track2)    = show track1 ++ ", " ++ show track2
    show (track1 :::: track2)    = "(" ++ show track1 ++ ")" ++ " || " ++ "(" ++ show track2 ++ ")"

-- actually no need to print indexes for extended tracks, as index manipulation will be available only for tracks
showIndex tr@EmptyET n    = show tr ++ showInd n 
showIndex tr@(PrimET _) n = show tr ++ showInd n
showIndex (track1 :++: track2) n = showIndex track1 n ++ ", " ++ showIndex track2 (n + lengthET track1)
showIndex (track1 :::: track2) n = "(" ++ showIndex track1 n ++ ")" ++ " || " ++ "(" ++ showIndex track2 (n + lengthET track1) ++ ")"

instance Show Music where
    show (Music track oct inst) = "octave: " ++ show oct ++ ", instument: " ++ show inst ++ "\n" ++ show track
    show (music1 ::: music2)    = show music1 ++ "\n" ++ show music2