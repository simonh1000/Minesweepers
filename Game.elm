module Game (init, update, view) where

import Html exposing (..)
import Html.Attributes exposing (type', style, src, rel, href, id)
import Set exposing (Set, singleton, union, member)
import Array exposing (Array, get)
import List exposing (foldl, filter)
import Effects exposing (Effects)
import Random exposing (Seed)

import Tile exposing (..)
import Board exposing (getNeighbours, isNeighbour)

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
    if model.game == Win || model.game == Lose
    then model
    else case action of
        Click i tileAction ->
            let (Just tile) = get i model.tiles
            in if   | tile.isRevealed -> model
                    | tile.isMine ->
                        { model
                        | game <- Lose
                        , tiles <- Array.map (\t -> {t | isRevealed <- t.isRevealed || t.isMine }) model.tiles
                        }
                    | otherwise ->
                        let
                            mapper tile =
                                if member tile.id (tilesToReveal model (singleton i) (singleton i))
                                then Tile.update tileAction tile
                                else tile
                            newTiles = Array.map mapper model.tiles
                        in  { model
                            | game <- if (List.all (\t -> t.isRevealed || t.isMine) <| Array.toList newTiles) then Win else InPlay
                            , tiles <- newTiles
                            }

-- on a blank reveal all surrounding squares, recursively
-- have ID,
-- from that return [ID - cols - 1, ID - cols, ID - cols + 1]

-- if square clicked has threatCount then this is the only one we will reveal

-- diff : Get the difference between the first set and the second. Keeps values that do not appear in the second set.
tilesToReveal : Model -> Set Int -> Set Int -> Set Int
tilesToReveal model tilesAcc cands =
    let
        go : Int -> Set Int -> Set Int
        go v acc =
            let (Just tile) = get v model.tiles
            in if tile.threatCount /= 0
                then acc
                else
                    let
                        candidates =
                            Set.fromList <| List.filter (isNeighbour model.rows model.cols v) (getNeighbours model.cols v)   -- [Int]
                        frontier = Set.diff candidates acc
                    in if Set.isEmpty frontier
                        then acc
                        else tilesToReveal model (union acc frontier) frontier

    in Set.foldl go tilesAcc cands

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
    -- , ("height", (toString (r * 50)) ++ "px")
    ]
