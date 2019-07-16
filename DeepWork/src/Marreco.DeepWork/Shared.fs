namespace Marreco.DeepWork


module Optics = 
    type Lens<'a, 'b> = ('a -> 'b) * ('b -> 'a -> 'a)
    type Prism<'a, 'b> = ('a -> 'b option) * ('b -> 'a -> 'a)


type TBD = Undefined // To be defined...


//Apply the events to the current state, bringing the aggregate state Up to Date
type ApplyEvents<'state, 'event> = 'state -> 'event -> 'state

type CommandHandler<'state, 'command, 'event, 'error> = 'state -> 'command -> Result<'event list, 'error>