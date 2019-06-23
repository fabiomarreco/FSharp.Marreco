namespace Marreco.DeepWork

type TBD = Undefined // To be defined...


//Apply the events to the current state, bringing the aggregate state Up to Date
type ApplyEvents<'state, 'event> = 'state -> 'event -> 'state

type CommandHandler<'state, 'command, 'event, 'error> = 'state -> 'command -> Result<'event list, 'error>