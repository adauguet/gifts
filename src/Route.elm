module Route exposing (parseFamily)

import Family exposing (Family)
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s)


parseFamily : Url -> List Family -> Maybe Family
parseFamily url families =
    let
        parsers : Parser (Family -> a) a
        parsers =
            families
                |> List.map
                    (\f ->
                        oneOf
                            [ map f (s "gifts" </> s f.path)
                            , map f (s f.path)
                            ]
                    )
                |> oneOf
    in
    parse parsers url
