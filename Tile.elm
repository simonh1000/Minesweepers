module Tile (Model, ID, Action, init, update, view) where

import Html exposing (..)
import Html.Attributes exposing (style, src, class)
import Html.Events exposing (onClick)


-- MODEL
type alias ID = Int

type alias Model =
    { id : ID
    , isRevealed : Bool
    , isMine : Bool
    , threatCount : Int
    }

init : Int -> Bool -> Int -> Model
init i m t =
    { id = i
    , isRevealed = False
    , isMine = m
    , threatCount = t
    }

-- UPDATE

type Action = Reveal

update : Action -> Model -> Model
update action model =
  case action of
    Reveal -> { model | isRevealed <- True }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    if | not model.isRevealed
            -> span [covered, onClick address Reveal] []
       | model.isRevealed && model.isMine
            -> img [imgStyle, src "./bomb.png"] []
       | otherwise
            -> span [class "revealed"]
                <| [text <| if model.threatCount > 0 then toString model.threatCount else ""]

covered : Attribute
covered =
  style
    [ ("background-color", "red")
    , ("cursor", "pointer")
    ]

imgStyle : Attribute
imgStyle =
  style
    [ ("width", "50px")
    , ("height", "46px")
    ]
