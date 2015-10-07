module Tile (Model, ID, Action, init, update, view) where

import Html exposing (..)
import Html.Attributes exposing (style, src, class)
import Html.Events exposing (on)
-- import Html.Events exposing (onClick)
import Json.Decode exposing ((:=), bool)

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
-- on : String -> Decoder a -> (a -> Message) -> Attribute

onClick : Signal.Address a -> Attribute
onClick address =
    on "click" ("ctrlKey" := bool) (\r -> Signal.message address r)

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
    , ("display", "block")
    , ("width", "50px")
    , ("height", "46px")
    , ("cursor", "pointer")
    ]

imgStyle : Attribute
imgStyle =
  style
    [ ("width", "50px")
    , ("height", "46px")
    ]
