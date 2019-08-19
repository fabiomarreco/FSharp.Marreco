module Work 

type DeepWork = string
type ShallowWork = string
type Offwork = TBD

type Work =
    | DeepWork    of DeepWork
    | ShallowWork of ShallowWork

type Engagement =
    | Deep    of DeepWork
    | Shallow of ShallowWork list
    | Offwork of Offwork
