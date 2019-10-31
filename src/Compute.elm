module Compute exposing (compute)

import Input exposing (Input(..), toPersons)
import Link exposing (Link, fromPersons)
import List.Extra as List
import Random exposing (Generator)
import Random.List


compute : List Input -> List Link -> Generator (Maybe (List Link))
compute inputs forbidden =
    inputs
        |> List.map toPersons
        |> List.concat
        |> List.permutations
        |> List.map fromPersons
        |> Random.List.shuffle
        |> Random.map (List.find (validate forbidden))


validate : List Link -> List Link -> Bool
validate forbidden solution =
    solution
        |> List.map (\link -> List.member link forbidden)
        |> List.any identity
        |> not
