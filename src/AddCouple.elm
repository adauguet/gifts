module AddCouple exposing (Model, Msg, init, update, view)

import Css
    exposing
        ( column
        , displayFlex
        , flexDirection
        , justifyContent
        , margin2
        , marginBottom
        , marginLeft
        , marginRight
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
    { first : String
    , second : String
    }


init : Model
init =
    { first = ""
    , second = ""
    }



-- update


type Msg
    = OnInputFirst String
    | OnInputSecond String


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnInputFirst string ->
            { model | first = string }

        OnInputSecond string ->
            { model | second = string }



-- view


view :
    Model
    -> (Msg -> parentMsg)
    -> (Input -> parentMsg)
    -> parentMsg
    -> Html parentMsg
view model toParentMsg onClickOK onClickCancel =
    div [ css [ displayFlex, flexDirection column ] ]
        [ div [ css [ marginBottom (rem 0.5) ] ] [ text "Ajouter un couple" ]
        , input [ css [ margin2 (rem 0.5) zero ], onInput (OnInputFirst >> toParentMsg) ] []
        , text "et"
        , input [ css [ margin2 (rem 0.5) zero ], onInput (OnInputSecond >> toParentMsg) ] []
        , div [ css [ displayFlex, justifyContent spaceBetween, margin2 (rem 0.5) zero ] ]
            [ button [ css [ width (pct 48) ], attribute model.first model.second onClickOK ] [ text "OK" ]
            , button [ css [ width (pct 48) ], onClick onClickCancel ] [ text "Annuler" ]
            ]
        ]


attribute : String -> String -> (Input -> parentMsg) -> Attribute parentMsg
attribute first second onClickConfirm =
    case validateInput first second of
        Nothing ->
            disabled True

        Just input ->
            onClick (onClickConfirm input)


validateInput : String -> String -> Maybe Input
validateInput first second =
    case ( first, second ) of
        ( "", _ ) ->
            Nothing

        ( _, "" ) ->
            Nothing

        ( name1, name2 ) ->
            Just (Couple name1 name2)
