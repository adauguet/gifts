module AddConstraint exposing (Model, Msg, init, update, view)

import Css
    exposing
        ( column
        , displayFlex
        , flexDirection
        , margin2
        , pct
        , rem
        , width
        , zero
        )
import Html.Styled exposing (Attribute, Html, button, div, option, select, text)
import Html.Styled.Attributes exposing (css, disabled, value)
import Html.Styled.Events exposing (onClick, onInput)
import Link exposing (Link)
import Person exposing (Person)



-- model


type alias Model =
    { persons : List Person
    , first : Person
    , second : Person
    }


init : Person -> List Person -> Model
init person persons =
    { persons = person :: persons
    , first = person
    , second = person
    }



-- update


type Msg
    = OnChooseFirst Person
    | OnChooseSecond Person


update : Msg -> Model -> Model
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
    case msg of
        OnChooseFirst person ->
            { model | first = person }

        OnChooseSecond person ->
            { model | second = person }



-- view


view :
    Model
    -> (Msg -> parentMsg)
    -> (Link -> parentMsg)
    -> parentMsg
    -> Html parentMsg
view model toParentMsg onClickOK onClickCancel =
    div [ css [ displayFlex, flexDirection column ] ]
        [ div [ css [ margin2 (rem 0.5) zero ] ] [ text "Ajouter une contrainte" ]
        , personSelect model.persons (OnChooseFirst >> toParentMsg)
        , text "ne doit pas offrir à :"
        , personSelect model.persons (OnChooseSecond >> toParentMsg)
        , div [ css [ displayFlex ] ]
            [ button [ css [ width (pct 50) ], attribute model.first model.second onClickOK ] [ text "OK" ]
            , button [ css [ width (pct 50) ], onClick onClickCancel ] [ text "Annuler" ]
            ]
        ]


personSelect : List Person -> (String -> parentMsg) -> Html parentMsg
personSelect persons onInputMsg =
    persons
        |> List.map (\person -> option [] [ text person ])
        |> select [ onInput onInputMsg ]


attribute : Person -> Person -> (Link -> parentMsg) -> Attribute parentMsg
attribute person1 person2 onClickConfirm =
    case validateLink person1 person2 of
        Nothing ->
            disabled True

        Just link ->
            onClick (onClickConfirm link)


validateLink : Person -> Person -> Maybe Link
validateLink person1 person2 =
    if person1 /= person2 then
        Just ( person1, person2 )

    else
        Nothing
