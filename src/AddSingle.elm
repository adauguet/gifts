module AddSingle exposing (Model, Msg, init, update, view)

import Css
    exposing
        ( column
        , displayFlex
        , flexDirection
        , pct
        , width
        )
import Html.Styled exposing (Attribute, Html, button, div, input, text)
import Html.Styled.Attributes exposing (css, disabled)
import Html.Styled.Events exposing (onClick, onInput)
import Input exposing (Input(..))



-- model


type alias Model =
    String


init : Model
init =
    ""



-- update


type Msg
    = OnInput String


update : Msg -> Model
update msg =
    case msg of
        OnInput string ->
            string



-- view


view :
    Model
    -> (Msg -> parentMsg)
    -> (Input -> parentMsg)
    -> parentMsg
    -> Html parentMsg
view model toParentMsg onClickOK onClickCancel =
    div [ css [ displayFlex, flexDirection column ] ]
        [ text "Ajouter une personne"
        , input [ onInput (OnInput >> toParentMsg) ] []
        , div [ css [ displayFlex ] ]
            [ button [ css [ width (pct 50) ], attribute model onClickOK ] [ text "OK" ]
            , button [ css [ width (pct 50) ], onClick onClickCancel ] [ text "Annuler" ]
            ]
        ]


attribute : String -> (Input -> parentMsg) -> Attribute parentMsg
attribute string onClickConfirm =
    case validateInput string of
        Nothing ->
            disabled True

        Just input ->
            onClick (onClickConfirm input)


validateInput : String -> Maybe Input
validateInput string =
    case string of
        "" ->
            Nothing

        name ->
            Just (Single name)
