module Game (init, update, view) where

import Html exposing (..)
import Html.Attributes exposing (type', style, src, rel, href, id)
import Set exposing (Set, singleton, union, member)
import Array exposing (Array, get, set)
import List exposing (foldl, filter)
import Effects exposing (Effects)
import Random exposing (Seed)

import Tile exposing (..)
import Board exposing (getNeighbours', isNeighbour)

-- MODEL

type GameState = InPlay | Win | Lose

type alias Model =
    { game : GameState
    , rows : Int
    , cols : Int
    , tiles : Array Tile.Model
    }

init : Int -> Int -> Seed -> Model
init r c seed =
    { game = InPlay
    , rows = r
    , cols = c
    , tiles = (Board.init r c seed)
    }

-- UPDATE

type Action
    = Click Tile.ID Tile.Action

update : Action -> Model -> (Model, Effects Action)
update a m = (updateInner a m, Effects.none)

updateInner : Action -> Model -> Model
updateInner action model =
    -- if game already over, do nothing
    if model.game == Win || model.game == Lose
    then model
    else case action of
        Click i tileAction ->
            -- examine the tile clicked upon
            let
                (Just tile) = get i model.tiles
            in if   | tile.isRevealed -> model -- already revealed
                    | tile.isMine ->           -- mine --> Lose
                        { model
                        | game <- Lose
                        , tiles <- Array.map (\t -> {t | isRevealed <- t.isRevealed || t.isMine }) model.tiles
                        }
                    | otherwise ->
                        let
                            newTiles = List.foldl (explorer model) model.tiles [i]
                        in  { model
                            | game <- if (List.all (\t -> t.isRevealed || t.isMine) <| Array.toList newTiles) then Win else InPlay
                            , tiles <- newTiles
                            }

-- explorer is passed list of next squares, including ones that might already have been explored
explorer : Model -> (Int -> Array Tile.Model -> Array Tile.Model)
explorer model =
    \v acc ->
        let
            (Just tile) = get v acc
            acc' = set v {tile | isRevealed <- True} acc
        in if tile.isRevealed || tile.threatCount /= 0
            then acc'
            else -- threatCount == 0, recurse further into map
                let
                    rawCandidates = getNeighbours' model.rows model.cols v
                    neighbouringCands = List.filter (isNeighbour model.rows model.cols v) rawCandidates
                in List.foldl (explorer model) acc' neighbouringCands

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    let
        extraElement = case model.game of
            Win -> [text "You Win :-)"]
            Lose -> [text "You Lose :-("]
            InPlay -> [text <| "Need to find: " ++ (toString <| Array.length <| Array.filter (\t -> t.isMine) model.tiles)]
    in
        div [] <|
            [ h1 [style [("text-align", "center")]] [text "Minesweeper"]
            , div [gameStyle model.rows model.cols, id "board"]
                [ div [] (Array.toList <| Array.map (viewTile address) model.tiles)
                , p [] extraElement
                ]
            ]

viewTile : Signal.Address Action -> Tile.Model -> Html
viewTile address tile =
  Tile.view (Signal.forwardTo address (Click tile.id)) tile

gameStyle : Int -> Int -> Attribute
gameStyle r c =
  style
    [ ("width", (toString (c * 50)) ++ "px")
    ]
