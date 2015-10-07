module Tile (Model, ID, Action(..), init, update, view) where

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
    , isMarked : Bool
    , isMine : Bool
    , threatCount : Int
    }

init : Int -> Bool -> Int -> Model
init i m t =
    { id = i
    , isRevealed = False
    , isMarked = False
    , isMine = m
    , threatCount = t
    }

-- UPDATE

type Action = Reveal | Mark

update : Action -> Model -> Model
update action model =
  case action of
    Reveal -> { model | isRevealed <- True, isMarked <- False }
    Mark   -> if model.isRevealed then model else { model | isMarked <- True }


-- VIEW

-- on : String -> Decoder a -> (a -> Message) -> Attribute
onClick : Signal.Address Action -> Attribute
onClick address =
    on
        "click"
        ("ctrlKey" := bool)
        (\r -> Signal.message address (if r then Mark else Reveal))

view : Signal.Address Action -> Model -> Html
view address model =
    if | model.isMarked
            -> span [class "marked", onClick address] [ text "?" ]
       | not model.isRevealed
            -> span [covered, onClick address] []
       | model.isRevealed && model.isMine
            -> img [imgStyle, src "./bomb.png"] []
       | otherwise
            -> span [class "revealed"]
                <| [text <| if model.threatCount > 0 then toString model.threatCount else ""]

covered : Attribute
covered =
  style
    [ ("background-color", "red")
    -- , ("display", "inline-block")
    -- , ("width", "50px")
    -- , ("height", "46px")
    -- , ("cursor", "pointer")
    ]

marked : Attribute
marked =
  style
    [ ("background-color", "blue")
    , ("display", "inline-block")
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
