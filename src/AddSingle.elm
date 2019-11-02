module AddSingle exposing (Model, Msg, init, update, view)

import Css
    exposing
        ( column
        , displayFlex
        , flexDirection
        , justifyContent
        , margin
        , margin2
        , marginTop
        , pct
        , rem
        , spaceBetween
        , width
        , zero
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
        [ div [ css [ margin2 (rem 0.5) zero ] ] [ text "Ajouter une personne" ]
        , input [ css [ margin2 (rem 0.5) zero ], onInput (OnInput >> toParentMsg) ] []
        , div [ css [ displayFlex, justifyContent spaceBetween, margin2 (rem 0.5) zero ] ]
            [ button [ css [ width (pct 48) ], attribute model onClickOK ] [ text "OK" ]
            , button [ css [ width (pct 48) ], onClick onClickCancel ] [ text "Annuler" ]
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
