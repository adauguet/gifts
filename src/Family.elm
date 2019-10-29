module Family exposing (Family, families)

import Input exposing (Input(..))


type alias Family =
    { name : String
    , path : String
    , members : List Input
    }


families : List Family
families =
    [ gerardin, dauguet ]


gerardin : Family
gerardin =
    { name = "Gérardin"
    , path = "gerardin"
    , members =
        [ Couple "Pierre" "Solène"
        , Single "Marie"
        , Couple "Benoît" "Camille"
        , Couple "Antoine" "Claire"
        , Couple "Benoit" "Emmanuelle"
        ]
    }


dauguet : Family
dauguet =
    { name = "Dauguet"
    , path = "dauguet"
    , members =
        [ Couple "Antoine" "Claire"
        , Couple "Louis-Marie" "Adeline"
        , Couple "Grégoire" "Claire"
        , Single "Bénédicte"
        , Single "Jean-Baptiste"
        ]
    }
