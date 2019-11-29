module Helpers exposing (intersect, shiftLeft, shuffle)

import Random exposing (Generator)
import Random.List
import Set


{-| source: elm-community/random-extra
The implementation in the current version 3.1.0 does not provide satisfying results.
This implementation is extracted from version 3.0.0 and uses Fisher Yates algorithm.
-}
shuffle : List a -> Generator (List a)
shuffle list =
    if List.isEmpty list then
        Random.constant list

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
        Random.map Tuple.first (helper ( [], list ))


shiftLeft : List a -> List a
shiftLeft list =
    case list of
        [] ->
            []

        head :: tail ->
            tail ++ [ head ]


intersect : List comparable -> List comparable -> List comparable
intersect list1 list2 =
    Set.intersect (Set.fromList list1) (Set.fromList list2)
        |> Set.toList
