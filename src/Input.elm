module Input exposing (Input(..), toCouples, toPerson, toPersons)

import Person exposing (Person)


type Input
    = Single Person
    | Couple Person Person


toPerson : Input -> List Person
toPerson input =
    case input of
        Single p ->
            [ p ]

        Couple p1 p2 ->
            [ p1, p2 ]


toPersons : List Input -> List Person
toPersons inputs =
    inputs
        |> List.map toPerson
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
