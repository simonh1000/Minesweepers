import Effects exposing (Never)
import Task
import StartApp
import StartApp.Simple exposing (start)

import Game exposing (init, update, view)

main =
    start
        { model = init 4 4
        , update = update
        , view = view
        }

-- app =
--   StartApp.start
--     { init = init
--     , update = update
--     , view = view
--     , inputs = []
--     }
--
-- main =
--   app.html
--
-- port startTime : Float
--
-- port tasks : Signal (Task.Task Never ())
-- port tasks =
--   app.tasks
