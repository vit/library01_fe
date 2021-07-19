module Route exposing (Route(..), fromUrl, toString)

import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Url exposing (Url)


-- ROUTING

type alias ItemId = String

type Route
    = Home
    | Item ItemId

fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser

parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        --, Parser.map TodoItem (s "" </> urlParser)
        , Parser.map Item urlParser
        ]

urlParser : Parser (ItemId -> a) a
urlParser =
    Parser.custom "ItemId" (\str -> Just str)





toString : Route -> String
toString page =
    "#/" ++ String.join "/" (routeToPieces page)

routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Home ->
            []
        Item s ->
            [ s ]
