import StartApp
import StartApp.Simple exposing (start)

import Effects exposing (Never)
import Task
import Random exposing (Seed)

import Game exposing (init, update, view)
import Tile

-- main =
--     start
--         { model = Tile.init 0 False 0
--         , update = Tile.update
--         , view = Tile.view
--         }

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
