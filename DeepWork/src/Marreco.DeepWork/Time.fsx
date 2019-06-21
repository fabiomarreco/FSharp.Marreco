#load "Time.fs"

open Marreco.DeepWork
open Time


let day = Date.today
let duration = Duration.fromMinutes 30
let periods = Period.splitDayIn duration day