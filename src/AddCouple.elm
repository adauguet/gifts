module AddCouple exposing (Model, Msg, init, update, view)

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
        [ text "Ajouter un couple"
        , input [ onInput (OnInputFirst >> toParentMsg) ] []
        , text "et"
        , input [ onInput (OnInputSecond >> toParentMsg) ] []
        , div [ css [ displayFlex ] ]
            [ button [ css [ width (pct 50) ], attribute model.first model.second onClickOK ] [ text "OK" ]
            , button [ css [ width (pct 50) ], onClick onClickCancel ] [ text "Annuler" ]
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
