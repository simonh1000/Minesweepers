module Board (init, getNeighbours, isNeighbour) where

import List exposing (foldl, filter, map)
import Set exposing (Set, member)
import Array exposing (Array)
import Random exposing (Seed)

import Tile exposing (Model)

init : Int -> Int -> Seed -> Array Model
init r c seed =
    let
        total = r * c
        -- generate : Generator a -> Seed -> ( a, Seed )
        -- using a set removes duplicates
        mines = Set.fromList <| fst <| Random.generate (intList (total // 5) total) seed
        makeSquare : Set Int -> Int -> Model
        makeSquare mines i =
            let
                neighbours = List.filter (isNeighbour r c i) (getNeighbours r c i)
                calcThreat = List.length <| List.filter (\n -> Set.member n mines) neighbours
            in Tile.init i (member i mines) calcThreat

    -- initialize : Int -> (Int -> a) -> Array a
    in Array.initialize (r * c) (makeSquare mines)


intList : Int -> Int -> Random.Generator (List Int)
intList n m =
    Random.list n (Random.int 0 m)


getNeighbours : Int -> Int -> Int -> List Int
getNeighbours r c i =
    let
        deltas =
            [ -c - 1, -c, -c + 1
            , -1, 1
            , c - 1, c, c + 1
            ]
    in filter (\x -> 0 <= x && x < r * c) <| map (\x -> i+x) deltas


isNeighbour : Int -> Int -> Int -> Int -> Bool
isNeighbour rows cols i c =
    if  | abs ((i `rem` cols) - (c `rem` cols)) > 1 -> False
        | otherwise -> True
