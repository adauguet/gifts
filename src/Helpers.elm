module Helpers exposing (shiftLeft, shuffle)

import Random exposing (Generator)
import Random.List


{-| source: elm-community/random-extra
The implementation in the current version 3.1.0 does not provide satisfying results.
This implementation is extracted from version 3.0.0 and uses Fisher Yates algorithm.
-}
shuffle : List a -> Generator (List a)
shuffle arr =
    if List.isEmpty arr then
        Random.constant arr

    else
        let
            helper : ( List a, List a ) -> Generator ( List a, List a )
            helper ( done, remaining ) =
                Random.List.choose remaining
                    |> Random.andThen
                        (\( m_val, shorter ) ->
                            case m_val of
                                Nothing ->
                                    Random.constant ( done, shorter )

                                Just val ->
                                    helper ( val :: done, shorter )
                        )
        in
        Random.map Tuple.first (helper ( [], arr ))


shiftLeft : List a -> List a
shiftLeft list =
    case list of
        [] ->
            []

        head :: tail ->
            tail ++ [ head ]
