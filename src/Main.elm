module Main exposing (main)

import Algo exposing (run)
import Browser exposing (Document)
import Css
    exposing
        ( Style
        , alignItems
        , alignSelf
        , backgroundColor
        , baseline
        , border
        , border2
        , border3
        , borderBottom3
        , borderColor
        , borderRadius
        , borderStyle
        , borderWidth4
        , bottom
        , boxShadow5
        , center
        , color
        , column
        , displayFlex
        , flexDirection
        , flexStart
        , focus
        , fontFamilies
        , fontSize
        , fontWeight
        , height
        , hidden
        , hover
        , int
        , left
        , margin
        , margin2
        , marginBottom
        , marginLeft
        , marginTop
        , none
        , outline
        , padding
        , padding2
        , pseudoElement
        , px
        , rem
        , rgb
        , right
        , sansSerif
        , serif
        , solid
        , textAlign
        , transparent
        , verticalAlign
        , visibility
        , visible
        , width
        , zero
        )
import Css.Global exposing (children, everything, global)
import Css.Transitions exposing (ease, transform3, transition)
import Helpers exposing (shiftLeft, shuffle)
import Html.Styled exposing (Attribute, Html, button, div, i, input, span, text, toUnstyled)
import Html.Styled.Attributes exposing (class, css, value)
import Html.Styled.Events exposing (onClick, onInput)
import Input exposing (Input(..), toCouples, toPersons)
import Person exposing (Person)
import Random exposing (generate)
import Theme



-- model


type alias Model =
    { inputs : List Input
    , input : String
    , inputLeft : String
    , inputRight : String
    , state : State
    }


type State
    = Default
    | AddSingle
    | AddCouple
    | Results (List Person)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputs = []
      , input = ""
      , inputLeft = ""
      , inputRight = ""
      , state = Default
      }
    , Cmd.none
    )



-- update


type Msg
    = OnInput String
    | OnInputLeft String
    | OnInputRight String
    | OnClickAddSingle
    | OnClickAddSingleCancel
    | OnClickAddSingleConfirm
    | OnClickAddCouple
    | OnClickAddCoupleCancel
    | OnClickAddCoupleConfirm
    | OnClickCompute
    | GotRandomPersons (List Person)
    | OnClickRestart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInput string ->
            ( { model | input = string }, Cmd.none )

        OnInputLeft string ->
            ( { model | inputLeft = string }, Cmd.none )

        OnInputRight string ->
            ( { model | inputRight = string }, Cmd.none )

        OnClickAddSingle ->
            ( { model | state = AddSingle }, Cmd.none )

        OnClickAddCouple ->
            ( { model | state = AddCouple }, Cmd.none )

        OnClickAddSingleCancel ->
            ( { model | state = Default, input = "" }, Cmd.none )

        OnClickAddCoupleCancel ->
            ( { model | state = Default, inputLeft = "", inputRight = "" }, Cmd.none )

        OnClickAddSingleConfirm ->
            case model.input of
                "" ->
                    ( model, Cmd.none )

                p ->
                    ( { model
                        | inputs = List.append model.inputs [ Single p ]
                        , input = ""
                        , state = Default
                      }
                    , Cmd.none
                    )

        OnClickAddCoupleConfirm ->
            case ( model.inputLeft, model.inputRight ) of
                ( "", _ ) ->
                    ( model, Cmd.none )

                ( _, "" ) ->
                    ( model, Cmd.none )

                ( p1, p2 ) ->
                    ( { model
                        | inputs = List.append model.inputs [ Couple p1 p2 ]
                        , inputLeft = ""
                        , inputRight = ""
                        , state = Default
                      }
                    , Cmd.none
                    )

        OnClickCompute ->
            ( model, generate GotRandomPersons (shuffle (toPersons model.inputs)) )

        GotRandomPersons persons ->
            let
                couples =
                    toCouples model.inputs

                chain =
                    run couples persons
            in
            if List.isEmpty chain then
                ( model, Cmd.none )

            else
                ( { model | state = Results chain }, Cmd.none )

        OnClickRestart ->
            ( { model
                | state = Default
                , input = ""
                , inputLeft = ""
                , inputRight = ""
                , inputs = []
              }
            , Cmd.none
            )



-- view


inputView : Input -> Html Msg
inputView input =
    div
        [ css
            [ displayFlex
            , alignItems center
            , height (px 34)
            , fontSize (px 18)
            ]
        ]
        (case input of
            Single p ->
                [ div [] [ text p ] ]

            Couple p1 p2 ->
                [ div [] [ text p1, text " et ", text p2 ] ]
        )


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
                    ]
                ]
                [ div [ css [ width (rem 8), textAlign right ] ] [ text a ]
                , div [ css [ margin2 zero (rem 0.5) ] ] [ text "offre à" ]
                , div [ css [ width (rem 8) ] ] [ text b ]
                ]
    in
    List.map format tuples


buttons : List Input -> Html Msg
buttons inputs =
    let
        visibilityValue =
            if List.length inputs > 1 then
                visibility visible

            else
                visibility hidden

        styles =
            css
                [ borderStyle none
                , padding zero
                , textAlign left
                , color Theme.textColor
                , height (rem 2)
                , fontSize (px 18)
                , backgroundColor transparent
                , marginTop (rem 0.5)
                , borderRadius (px 5)
                , hover [ color Theme.hoverColor ]
                , focus [ outline none ]
                , pseudoElement "-moz-focus-inner" [ border zero ]
                ]
    in
    div [ css [ displayFlex, flexDirection column, marginTop (rem 0.5) ] ]
        [ button [ onClick OnClickAddSingle, styles ] [ text "+ Ajouter une personne" ]
        , button [ onClick OnClickAddCouple, styles ] [ text "+ Ajouter un couple" ]
        , button
            [ onClick OnClickCompute
            , css
                [ marginTop (rem 1)
                , visibilityValue
                , height (rem 3)
                , fontSize (px 18)
                ]
            ]
            [ text "Calculer" ]
        ]


inputStyles : Attribute Msg
inputStyles =
    css
        [ width (rem 7)
        , fontSize (px 18)
        , border zero
        , borderBottom3 (px 1) solid Theme.textColor
        , color Theme.textColor
        , outline none
        , padding zero
        , margin zero
        , color Theme.textColor
        , backgroundColor transparent
        ]


buttonStyles : Attribute Msg
buttonStyles =
    css
        [ marginLeft (rem 0.5)
        , fontSize (px 18)
        ]


view : Model -> Document Msg
view model =
    { title = ""
    , body =
        [ global
            [ everything
                [ fontFamilies [ "Roboto", .value sansSerif ]
                ]
            ]
        , div [ css [ padding (rem 1) ] ]
            [ case model.state of
                Default ->
                    div [ css [ displayFlex, alignItems center, flexDirection column ] ]
                        [ div [] (List.map inputView model.inputs)
                        , buttons model.inputs
                        ]

                AddSingle ->
                    div [ css [ displayFlex, alignItems center, flexDirection column ] ]
                        [ div [] (List.map inputView model.inputs)
                        , div [ css [ displayFlex, height (px 34) ] ]
                            [ input [ value model.input, onInput OnInput, inputStyles ] []
                            , button [ onClick OnClickAddSingleConfirm, buttonStyles ] [ text "OK" ]
                            , button [ onClick OnClickAddSingleCancel, buttonStyles ] [ text "Annuler" ]
                            ]
                        , buttons model.inputs
                        ]

                AddCouple ->
                    div [ css [ displayFlex, alignItems center, flexDirection column ] ]
                        [ div [] (List.map inputView model.inputs)
                        , div [ css [ displayFlex, height (px 34) ] ]
                            [ div [ css [ displayFlex ] ]
                                [ input [ value model.inputLeft, onInput OnInputLeft, inputStyles ] []
                                , div
                                    [ css
                                        [ margin2 zero (rem 0.5)
                                        , alignSelf center
                                        , color Theme.textColor
                                        , fontSize (px 18)
                                        ]
                                    ]
                                    [ text "et" ]
                                , input [ value model.inputRight, onInput OnInputRight, inputStyles ] []
                                ]
                            , button [ onClick OnClickAddCoupleConfirm, buttonStyles ] [ text "OK" ]
                            , button [ onClick OnClickAddCoupleCancel, buttonStyles ] [ text "Annuler" ]
                            ]
                        , buttons model.inputs
                        ]

                Results persons ->
                    div [ css [ displayFlex, alignItems center, flexDirection column ] ]
                        [ div [] (results persons)
                        , div [ css [ marginTop (rem 2) ] ]
                            [ button
                                [ onClick OnClickRestart
                                , css
                                    [ fontSize (px 18)
                                    , height (rem 3)
                                    ]
                                ]
                                [ text "Recommencer" ]
                            ]
                        ]
            ]
        ]
            |> List.map toUnstyled
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
