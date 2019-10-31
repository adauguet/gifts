module Input exposing (Input(..), toCouples, toPersons)

import Person exposing (Person)


type Input
    = Single Person
    | Couple Person Person


toPersons : Input -> List Person
toPersons input =
    case input of
        Single p ->
            [ p ]

        Couple p1 p2 ->
            [ p1, p2 ]


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
