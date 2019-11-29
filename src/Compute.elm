module Compute exposing (computeConflicts, randomize, swap)

import Input exposing (Input(..))
import Link exposing (Link, fromPersons)
import List.Extra as List
import Person exposing (Person)
import Random exposing (Generator)
import Random.List
import Set


swap : List Person -> Generator (List Person)
swap persons =
    let
        index =
            Random.int 0 (List.length persons - 1)
    in
    Random.map2 (\i j -> List.swapAt i j persons) index index


computeConflicts : List Person -> List Link -> Int
computeConflicts persons forbidden =
    persons
        |> Link.fromPersons
        |> Set.fromList
        |> Set.intersect (forbidden |> Set.fromList)
        |> Set.size


randomize : List Person -> Generator (List Person)
randomize persons =
    Random.List.shuffle persons
