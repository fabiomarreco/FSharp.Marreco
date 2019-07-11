module Work 

type DeepWork = TBD
type ShallowWork = TBD
type Offwork = TBD

type Work =
    | DeepWork    of DeepWork
    | ShallowWork of ShallowWork

type Engagement =
    | Deep    of DeepWork
    | Shallow of ShallowWork list
    | Offwork of Offwork
