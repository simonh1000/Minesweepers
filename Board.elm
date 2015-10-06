module Board (init, getNeighbours, isNeighbour) where

import Array exposing (Array, initialize)
import Set exposing (Set, member)

import Tile exposing (Model)

-- initialize : Int -> (Int -> a) -> Array a
init : Int -> Int -> Set Int -> Array Model
init r c mines =
    let
        makeSquare : Set Int -> Int -> Model
        makeSquare mines i =
            let
                neighbours = List.filter (isNeighbour r c i) (getNeighbours c i)
                calcThreat = List.length <| List.filter (\n -> Set.member n mines) neighbours
            in
                { id = i
                , isRevealed = False
                , isMine = member i mines
                , threatCount = calcThreat
                }

    in initialize (r * c) (makeSquare mines)



getNeighbours : Int -> Int -> List Int
getNeighbours cols i =
    [ i - cols - 1
    , i - cols
    , i - cols + 1
    , i - 1
    , i + 1
    , i + cols - 1
    , i + cols
    , i + cols + 1
    ]

isNeighbour : Int -> Int -> Int -> Int -> Bool
isNeighbour rows cols i c =
    if  | c < 0 || c >= rows * cols -> False
        | abs ((i `rem` cols) - (c `rem` cols)) > 1 -> False
        | otherwise -> True
