#load "Time.fs"

open Time


let day = Date.today
let duration = Duration.fromMinutes 30
let periods = Period.splitDayInPeriodsOf duration day