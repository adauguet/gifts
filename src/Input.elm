module Input exposing (Input(..), toCouples, toPersons)

import Person exposing (Person)


type Input
    = Single Person
    | Couple Person Person


toPersons : List Input -> List Person
toPersons inputs =
    let
        inputToPersons input =
            case input of
                Single p ->
                    [ p ]

                Couple p1 p2 ->
                    [ p1, p2 ]
    in
    inputs
        |> List.map inputToPersons
        |> List.concat


toCouple : Input -> Maybe ( Person, Person )
toCouple input =
    case input of
        Single _ ->
            Nothing

        Couple p1 p2 ->
            Just ( p1, p2 )


toCouples : List Input -> List ( Person, Person )
toCouples inputs =
    inputs
        |> List.map toCouple
        |> List.filterMap identity
