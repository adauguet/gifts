module Algo exposing (run)

import Person exposing (Person, canOffer)


type alias Data =
    { chain : List Person
    , pool : List Person
    , bin : List Person
    }


compute : List ( Person, Person ) -> Data -> Data
compute pairs { chain, pool, bin } =
    case pool of
        [] ->
            { chain = chain, pool = pool, bin = bin }

        pick :: tail ->
            case chain of
                [] ->
                    compute pairs { chain = [ pick ], pool = tail, bin = [] }

                last :: _ ->
                    if canOffer pairs last pick then
                        compute pairs { chain = pick :: chain, pool = List.append tail bin, bin = [] }

                    else
                        compute pairs { chain = chain, pool = tail, bin = pick :: bin }


close : List ( Person, Person ) -> List Person -> List Person
close pairs chain =
    case ( List.head chain, List.head (List.reverse chain) ) of
        ( Just first, Just last ) ->
            if canOffer pairs first last then
                chain

            else
                case List.reverse chain of
                    a :: b :: tail ->
                        List.reverse (b :: a :: tail)

                    _ ->
                        []

        _ ->
            chain


run : List ( Person, Person ) -> List Person -> List Person
run pairs persons =
    let
        result =
            compute pairs { chain = [], pool = persons, bin = [] }
    in
    case ( result.bin, result.pool ) of
        ( [], [] ) ->
            close pairs result.chain

        _ ->
            []
