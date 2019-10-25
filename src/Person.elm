module Person exposing (Person, canOffer, toPersons)


type alias Person =
    String


toPersons : List ( Person, Person ) -> List Person
toPersons pairs =
    pairs
        |> List.map (\( a, b ) -> [ a, b ])
        |> List.concat


canOffer : List ( Person, Person ) -> Person -> Person -> Bool
canOffer pairs p1 p2 =
    not (List.member ( p1, p2 ) pairs || List.member ( p2, p1 ) pairs)
