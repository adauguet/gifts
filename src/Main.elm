module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Html.Styled exposing (Html, div)
import Url exposing (Url)


type alias Model =
    {}


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url _ =
    let
        _ =
            Debug.log "init url" url
    in
    ( {}, Cmd.none )


type Msg
    = NoOp
    | OnUrlRequest UrlRequest
    | OnUrlChange Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = ""
    , body = []
    }


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
