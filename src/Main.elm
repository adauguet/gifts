module Main exposing (Model, Msg, init, update, view)

import AddCouple
import AddSingle
import Browser exposing (Document)
import Compute exposing (compute)
import Css
    exposing
        ( alignItems
        , center
        , column
        , displayFlex
        , flexDirection
        , fontFamilies
        , justifyContent
        , margin2
        , none
        , padding
        , rem
        , right
        , sansSerif
        , textAlign
        , width
        , zero
        )
import Css.Global exposing (everything, global)
import Helpers exposing (shiftLeft, shuffle)
import Html.Styled exposing (Html, button, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css, disabled)
import Html.Styled.Events exposing (onClick)
import Input exposing (Input(..), toCouples, toPersons)
import Link exposing (Link)
import Person exposing (Person)
import Random exposing (generate)
import Set



-- model


type alias Model =
    { inputs : List Input
    , state : State
    }


type State
    = Default
    | AddSingle AddSingle.Model
    | AddCouple AddCouple.Model
    | Results (List Link)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputs = []
      , state = Default
      }
    , Cmd.none
    )



-- update


type Msg
    = OnClickAddSingle
    | AddSingleMsg AddSingle.Msg
    | OnClickAddInputAdd Input
    | OnClickAddInputCancel
    | OnClickAddCouple
    | AddCoupleMsg AddCouple.Msg
    | OnClickCompute
    | OnClickRestart
    | GotResults (Maybe (List Link))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.state, msg ) of
        ( Default, OnClickAddSingle ) ->
            ( { model | state = AddSingle AddSingle.init }, Cmd.none )

        ( Default, OnClickAddCouple ) ->
            ( { model | state = AddCouple AddCouple.init }, Cmd.none )

        ( AddSingle _, AddSingleMsg subMsg ) ->
            ( { model | state = AddSingle (AddSingle.update subMsg) }, Cmd.none )

        ( AddCouple subModel, AddCoupleMsg subMsg ) ->
            ( { model | state = AddCouple (AddCouple.update subMsg subModel) }, Cmd.none )

        ( _, OnClickAddInputAdd input ) ->
            case alreadyExisting model.inputs input of
                [] ->
                    ( { model | inputs = input :: model.inputs, state = Default }, Cmd.none )

                other ->
                    let
                        _ =
                            Debug.log "Les prénoms suivants sont déjà utilisés :" other
                    in
                    ( model, Cmd.none )

        ( _, OnClickAddInputCancel ) ->
            ( { model | state = Default }, Cmd.none )

        ( Default, OnClickCompute ) ->
            let
                couples : List Link
                couples =
                    model.inputs
                        |> List.map Link.fromInput
                        |> List.concat
            in
            ( model, generate GotResults (compute model.inputs couples) )

        ( Default, GotResults maybeResult ) ->
            case maybeResult of
                Nothing ->
                    ( model, Cmd.none )

                Just links ->
                    ( { model | state = Results links }, Cmd.none )

        ( Results _, OnClickRestart ) ->
            ( { model
                | state = Default
                , inputs = []
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


alreadyExisting : List Input -> Input -> List Person
alreadyExisting inputs input =
    let
        list =
            inputs
                |> List.map toPersons
                |> List.concat
                |> Set.fromList

        persons =
            input
                |> toPersons
                |> Set.fromList
    in
    list
        |> Set.intersect persons
        |> Set.toList



-- view


inputView : Input -> Html Msg
inputView input =
    div
        [ css
            [ displayFlex
            , alignItems center
            ]
        ]
        (case input of
            Single p ->
                [ div [] [ text p ] ]

            Couple p1 p2 ->
                [ div [] [ text p1, text " et ", text p2 ] ]
        )


results : List Link -> List (Html Msg)
results links =
    let
        format ( a, b ) =
            div
                [ css [ displayFlex ] ]
                [ div [ css [ width (rem 8), textAlign right ] ] [ text a ]
                , div [ css [ margin2 zero (rem 0.5) ] ] [ text "offre à" ]
                , div [ css [ width (rem 8) ] ] [ text b ]
                ]
    in
    List.map format links


buttons : List Input -> Html Msg
buttons inputs =
    div [ css [ displayFlex, flexDirection column ] ]
        [ button [ onClick OnClickAddSingle ] [ text "Ajouter une personne" ]
        , button [ onClick OnClickAddCouple ] [ text "Ajouter un couple" ]
        , button [ onClick OnClickCompute, disabled (List.length inputs <= 1) ] [ text "Calculer" ]
        ]


body : Model -> List (Html Msg)
body model =
    [ global [ everything [ fontFamilies [ "Roboto", .value sansSerif ] ] ]
    , div [ css [ padding (rem 1), displayFlex, justifyContent center ] ]
        [ case model.state of
            Default ->
                div [ css [ displayFlex, alignItems center, flexDirection column ] ]
                    [ div [] (List.map inputView model.inputs)
                    , buttons model.inputs
                    ]

            AddSingle m ->
                AddSingle.view m AddSingleMsg OnClickAddInputAdd OnClickAddInputCancel

            AddCouple m ->
                AddCouple.view m AddCoupleMsg OnClickAddInputAdd OnClickAddInputCancel

            Results links ->
                div [ css [ displayFlex, alignItems center, flexDirection column ] ]
                    [ div [] (results links)
                    , button [ onClick OnClickRestart ] [ text "Recommencer" ]
                    ]
        ]
    ]


view : Model -> Document Msg
view model =
    { title = ""
    , body = body model |> List.map toUnstyled
    }



-- main


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
