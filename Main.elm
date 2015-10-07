import Effects exposing (Never)
import Task
import StartApp
import StartApp.Simple exposing (start)
import Random exposing (Seed)

import Game exposing (init, update, view)

startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime

app =
  StartApp.start
    { init = (init 8 8 startTimeSeed, Effects.none)
    , update = update
    , view = view
    , inputs = []
    }

main =
  app.html

port startTime : Float

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
