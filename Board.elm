module Board (init, getNeighbours, isNeighbour) where

import Array exposing (Array, initialize)
import Set exposing (Set, member)
import Random exposing (Seed)

import Tile exposing (Model)

-- initialize : Int -> (Int -> a) -> Array a
init : Int -> Int -> Seed -> Array Model
init r c seed =
    let
        total = r * c
        mines = Set.fromList <| fst <| Random.generate (intList (total // 5) total) seed
        -- mines = Set.fromList <| fst <| Random.generate (intList 5 (r*c)) (Random.initialSeed 54784)
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

intList : Int -> Int -> Random.Generator (List Int)
intList n m =
    Random.list n (Random.int 0 m)
-- generate : Generator a -> Seed -> ( a, Seed )

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
