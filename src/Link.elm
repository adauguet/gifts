module Link exposing (Link, fromInputs, fromPersons)

import Helpers exposing (shiftLeft)
import Input exposing (Input(..))
import Person exposing (Person)


type alias Link =
    ( String, String )


fromInput : Input -> List Link
fromInput input =
    case input of
        Single _ ->
            []

        Couple name1 name2 ->
            [ ( name1, name2 ), ( name2, name1 ) ]


fromInputs : List Input -> List Link
fromInputs inputs =
    inputs
        |> List.map fromInput
        |> List.concat


fromPersons : List Person -> List Link
fromPersons persons =
    List.map2 Tuple.pair persons (shiftLeft persons)
