module Work 


type DeepWork = TBD
type ShallowWork = TBD
type Offwork = TBD

type Work =
    | Deep    of DeepWork
    | Shallow of ShallowWork

type Assignment =
    | Deep    of DeepWork
    | Shallow of ShallowWork list
    | Offwork of Offwork