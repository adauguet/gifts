module Main exposing (main)

import AddConstraint
import AddCouple
import AddSingle
import Browser exposing (Document)
import Compute exposing (computeConflicts, randomize, swap)
import Css
    exposing
        ( alignItems
        , baseline
        , center
        , column
        , displayFlex
        , flexDirection
        , fontFamilies
        , fontSize
        , justifyContent
        , margin2
        , marginBottom
        , marginTop
        , none
        , padding
        , px
        , rem
        , right
        , sansSerif
        , textAlign
        , width
        , zero
        )
import Css.Global exposing (everything, global)
import Family exposing (Family, families)
import Helpers exposing (intersect)
import Html.Styled exposing (Html, button, div, input, text, toUnstyled)
import Html.Styled.Attributes exposing (checked, css, disabled, type_)
import Html.Styled.Events exposing (onCheck, onClick)
import Input exposing (Input(..), toPerson, toPersons)
import Link exposing (Link)
import Person exposing (Person)
import Random exposing (generate)



-- model


type alias Model =
    { inputs : List Input
    , constraints : List Link
    , couplesAreConstraints : Bool
    , state : State
    }


type State
    = Home
    | AddSingle AddSingle.Model
    | AddCouple AddCouple.Model
    | LoadFamily (List Family)
    | AddConstraint AddConstraint.Model
    | Computing Data
    | NotFound
    | Results (List Link)


type alias Data =
    { solution : List Person
    , avoid : List Link
    , conflicts : Int
    , count : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputs = []
      , constraints = []
      , couplesAreConstraints = True
      , state = Home
      }
    , Cmd.none
    )



-- update


type Msg
    = OnClickAddSingle
    | OnClickAddCouple
    | OnClickLoadFamily
    | OmToggleCouplesConstraints Bool
    | OnClickAddConstraint
    | OnClickAddInputAdd Input
    | OnLoadFamily Family
    | OnClickAddConstraintAdd Link
    | OnClickCancel
    | OnClickCompute
    | OnClickRestart
    | OnRandomizePersons (List Person)
    | OnSwap (List Person)
    | AddSingleMsg AddSingle.Msg
    | AddCoupleMsg AddCouple.Msg
    | AddConstraintMsg AddConstraint.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.state, msg ) of
        ( Home, OnClickAddSingle ) ->
            ( { model | state = AddSingle AddSingle.init }, Cmd.none )

        ( Home, OnClickAddCouple ) ->
            ( { model | state = AddCouple AddCouple.init }, Cmd.none )

        ( Home, OnClickLoadFamily ) ->
            ( { model | state = LoadFamily families }, Cmd.none )

        ( Home, OmToggleCouplesConstraints checked ) ->
            ( { model | couplesAreConstraints = checked }, Cmd.none )

        ( Home, OnClickAddConstraint ) ->
            case toPersons model.inputs of
                head :: tail ->
                    ( { model | state = AddConstraint (AddConstraint.init head tail) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( AddSingle _, AddSingleMsg subMsg ) ->
            ( { model | state = AddSingle (AddSingle.update subMsg) }, Cmd.none )

        ( AddCouple subModel, AddCoupleMsg subMsg ) ->
            ( { model | state = AddCouple (AddCouple.update subMsg subModel) }, Cmd.none )

        ( LoadFamily _, OnLoadFamily family ) ->
            ( { model | state = Home, inputs = family.members, constraints = [] }, Cmd.none )

        ( AddConstraint subModel, AddConstraintMsg subMsg ) ->
            ( { model | state = AddConstraint (AddConstraint.update subMsg subModel) }, Cmd.none )

        ( _, OnClickAddInputAdd input ) ->
            case alreadyExisting model.inputs input of
                [] ->
                    ( { model | inputs = model.inputs ++ [ input ], state = Home }, Cmd.none )

                other ->
                    let
                        _ =
                            Debug.log "Les prénoms suivants sont déjà utilisés :" other
                    in
                    ( model, Cmd.none )

        ( _, OnClickCancel ) ->
            ( { model | state = Home }, Cmd.none )

        ( AddConstraint _, OnClickAddConstraintAdd link ) ->
            ( { model | state = Home, constraints = model.constraints ++ [ link ] }, Cmd.none )

        ( _, OnClickCompute ) ->
            compute model

        ( _, OnRandomizePersons persons ) ->
            let
                avoid : List Link
                avoid =
                    if model.couplesAreConstraints then
                        model.constraints ++ Link.fromInputs model.inputs

                    else
                        model.constraints

                conflicts : Int
                conflicts =
                    computeConflicts persons avoid
            in
            if conflicts == 0 then
                ( { model | state = Results (Link.fromPersons persons) }, Cmd.none )

            else
                ( { model
                    | state =
                        Computing
                            { solution = persons
                            , avoid = avoid
                            , conflicts = conflicts
                            , count = 1
                            }
                  }
                , generate OnSwap (swap persons)
                )

        ( Computing current, OnSwap newPersons ) ->
            let
                conflicts =
                    computeConflicts current.solution current.avoid
            in
            if conflicts == 0 then
                ( { model | state = Results (Link.fromPersons current.solution) }, Cmd.none )

            else if current.count < 1000 then
                if conflicts <= current.conflicts then
                    ( { model | state = Computing { current | solution = newPersons, count = current.count + 1 } }
                    , generate OnSwap (swap newPersons)
                    )

                else
                    ( { model | state = Computing { current | count = current.count + 1 } }
                    , generate OnSwap (swap current.solution)
                    )

            else
                ( { model | state = NotFound }, Cmd.none )

        ( _, OnClickRestart ) ->
            init ()

        _ ->
            ( model, Cmd.none )


compute : Model -> ( Model, Cmd Msg )
compute model =
    ( model
    , model.inputs
        |> toPersons
        |> randomize
        |> generate OnRandomizePersons
    )


alreadyExisting : List Input -> Input -> List Person
alreadyExisting inputs input =
    intersect (toPersons inputs) (toPerson input)



-- view


inputView : Input -> Html Msg
inputView input =
    div
        [ css
            [ displayFlex
            , alignItems center
            , marginBottom (rem 0.5)
            ]
        ]
        (case input of
            Single p ->
                [ div [] [ text p ] ]

            Couple p1 p2 ->
                [ div [] [ text p1, text " et ", text p2 ] ]
        )


constraintView : Link -> Html Msg
constraintView link =
    div [ css [ displayFlex, alignItems center ] ]
        [ text (Tuple.first link)
        , text " -/-> "
        , text (Tuple.second link)
        ]


results : List Link -> List (Html Msg)
results links =
    let
        format ( a, b ) =
            div
                [ css [ displayFlex, marginBottom (rem 0.5) ] ]
                [ div [ css [ width (rem 8), textAlign right ] ] [ text a ]
                , div [ css [ margin2 zero (rem 0.5) ] ] [ text "offre à" ]
                , div [ css [ width (rem 8) ] ] [ text b ]
                ]
    in
    List.map format links


body : Model -> List (Html Msg)
body model =
    [ global [ everything [ fontFamilies [ "Roboto", .value sansSerif ] ] ]
    , div [ css [ padding (rem 1), displayFlex, justifyContent center ] ]
        [ case model.state of
            Home ->
                div [ css [ displayFlex, flexDirection column ] ]
                    [ div [ css [ displayFlex, flexDirection column ] ]
                        [ div [ css [ marginBottom (rem 0.5) ] ] [ text "Personnes" ]
                        , div [] (List.map inputView model.inputs)
                        , button [ css [ marginBottom (rem 0.5) ], onClick OnClickAddSingle ] [ text "Ajouter une personne" ]
                        , button [ css [ marginBottom (rem 0.5) ], onClick OnClickAddCouple ] [ text "Ajouter un couple" ]
                        , button [ onClick OnClickLoadFamily ] [ text "Charger une famille" ]
                        ]
                    , div [ css [ displayFlex, flexDirection column, marginTop (rem 1) ] ]
                        [ div [ css [ marginBottom (rem 0.5) ] ] [ text "Contraintes" ]
                        , div [ css [ marginBottom (rem 0.5), displayFlex, alignItems center ] ]
                            [ input [ type_ "checkbox", checked model.couplesAreConstraints, onCheck OmToggleCouplesConstraints ] []
                            , div [ css [ fontSize (px 13) ] ] [ text "Pas entre conjoints" ]
                            ]
                        , div [] (List.map constraintView model.constraints)
                        , button [ onClick OnClickAddConstraint ] [ text "Ajouter une contrainte" ]
                        ]
                    , div [ css [ displayFlex, flexDirection column, marginTop (rem 1) ] ]
                        [ button
                            [ onClick OnClickCompute
                            , disabled (List.length model.inputs <= 1)
                            ]
                            [ text "Calculer" ]
                        ]
                    ]

            AddSingle m ->
                AddSingle.view m AddSingleMsg OnClickAddInputAdd OnClickCancel

            AddCouple m ->
                AddCouple.view m AddCoupleMsg OnClickAddInputAdd OnClickCancel

            LoadFamily families ->
                div [ css [ displayFlex, flexDirection column ] ]
                    [ div [ css [ marginBottom (rem 0.5) ] ] [ text "Charger une famille" ]
                    , div [ css [ displayFlex, flexDirection column ] ]
                        (List.map
                            (\family ->
                                button
                                    [ onClick (OnLoadFamily family)
                                    , css [ margin2 (rem 0.5) zero ]
                                    ]
                                    [ text family.name ]
                            )
                            families
                        )
                    , button
                        [ onClick OnClickCancel
                        , css [ marginTop (rem 1) ]
                        ]
                        [ text "Annuler" ]
                    ]

            AddConstraint m ->
                AddConstraint.view m AddConstraintMsg OnClickAddConstraintAdd OnClickCancel

            Computing current ->
                div [] [ text (String.fromInt current.count) ]

            NotFound ->
                div [ css [ displayFlex, flexDirection column ] ]
                    [ text "Pas de résultat"
                    , div [ css [ displayFlex, flexDirection column, marginTop (rem 1) ] ]
                        [ button [ onClick OnClickCompute, css [ marginBottom (rem 0.5) ] ] [ text "Recalculer" ]
                        , button [ onClick OnClickRestart ] [ text "Recommencer" ]
                        ]
                    ]

            Results links ->
                div [ css [ displayFlex, flexDirection column, justifyContent center, alignItems center ] ]
                    [ div [] (results links)
                    , div [ css [ displayFlex, flexDirection column, marginTop (rem 1) ] ]
                        [ button [ onClick OnClickCompute, css [ marginBottom (rem 0.5) ] ] [ text "Recalculer" ]
                        , button [ onClick OnClickRestart ] [ text "Recommencer" ]
                        ]
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
