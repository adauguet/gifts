module Main exposing (..)

import Algo exposing (run)
import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Css
    exposing
        ( center
        , displayFlex
        , fontSize
        , height
        , justifyContent
        , margin2
        , px
        , rem
        , right
        , textAlign
        , width
        , zero
        )
import Family exposing (Family, families)
import Helpers exposing (shiftLeft, shuffle)
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Input exposing (Input, toCouples, toPersons)
import Person exposing (Person)
import Random exposing (generate)
import Url exposing (Url)
import Url.Parser exposing (Parser)


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url _ =
    let
        _ =
            Debug.log "url" url

        _ =
            Debug.log "family" (parseFamilly url)
    in
    case parseFamilly url of
        Just familly ->
            ( Loading, generate (GotRandomPersons familly.members) (shuffle (toPersons familly.members)) )

        Nothing ->
            ( Loading, Cmd.none )



-- model


type Model
    = Loading
    | Error
    | Result (List Person)


parseFamilly : Url -> Maybe Family
parseFamilly url =
    let
        parsers : Parser (Family -> a) a
        parsers =
            families
                |> List.map (\f -> Url.Parser.map f (Url.Parser.s f.path))
                |> Url.Parser.oneOf
    in
    Url.Parser.parse parsers url



-- update


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | GotRandomPersons (List Input) (List Person)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
    case msg of
        OnUrlRequest urlRequest ->
            ( model, Cmd.none )

        OnUrlChange url ->
            ( model, Cmd.none )

        GotRandomPersons inputs persons ->
            let
                couples =
                    toCouples inputs

                chain =
                    run couples persons
            in
            if List.isEmpty chain then
                ( Error, Cmd.none )

            else
                ( Result chain, Cmd.none )



--view


results : List Person -> List (Html Msg)
results persons =
    let
        tuples =
            List.map2 Tuple.pair persons (shiftLeft persons)

        format ( a, b ) =
            div
                [ css
                    [ displayFlex
                    , height (px 34)
                    , fontSize (px 18)
                    , justifyContent center
                    ]
                ]
                [ div [ css [ width (rem 8), textAlign right ] ] [ text a ]
                , div [ css [ margin2 zero (rem 0.5) ] ] [ text "offre à" ]
                , div [ css [ width (rem 8) ] ] [ text b ]
                ]
    in
    List.map format tuples


view : Model -> Document Msg
view model =
    { title = ""
    , body =
        (case model of
            Loading ->
                []

            Error ->
                []

            Result persons ->
                results persons
        )
            |> List.map toUnstyled
    }



-- main


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
