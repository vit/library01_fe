module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, text, nav, ul, li, a, b, h1)
--import Html.Events exposing (onClick)
import Html.Attributes exposing (class, href)
import Url
import Http

--import Json.Decode exposing (Decoder, field, string)
import Json.Decode as Decode
    exposing
        ( Decoder
        , decodeString
        , field
        , int
        , list
        , map3
        , string
        )

import Route --exposing (..)
import Types exposing (..)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ({key = key, url = url, page = HomePage}, updateUrlData url)


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UrlChanged url ->
        ({model | url = url}, updateUrlData url)

    ItemDataReceived result ->
        case result of
            Ok itemData ->
                ( {model | page = ItemPage itemData}, Cmd.none )
            Err httpError ->
                --( model, Cmd.none )
                ( {model | page = ErrorPage (buildErrorMessage httpError) }, Cmd.none )

    ItemsListDataReceived result ->
        case result of
            Ok itemsData ->
                ( {model | page = ItemsListPage itemsData}, Cmd.none )
            Err httpError ->
                ( model, Cmd.none )

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )
        Browser.External href ->
          ( model, Nav.load href )


updateUrlData : Url.Url -> Cmd Msg
updateUrlData url =
          let
            maybeRoute = (Route.fromUrl url)
          in
            case maybeRoute of
                Just route ->
                    case route of
                        Route.Home -> getItemsList
                        Route.Item id -> getItem id
                Nothing -> Cmd.none


urlBase = "https://jsonplaceholder.typicode.com/posts/"

getItem : String -> Cmd Msg
getItem id =
    Http.get
        { url = urlBase ++ id
        --, expect = Http.expectString ItemDataReceived
        --expect = Http.expectJson DataReceived (list postDecoder)
        , expect = Http.expectJson ItemDataReceived postDecoder
        }

getItemsList : Cmd Msg
getItemsList =
    Http.get
        { url = urlBase
        --, expect = Http.expectString ItemsListDataReceived
        , expect = Http.expectJson ItemsListDataReceived (list postDecoder)
        }

postDecoder : Decoder Post
postDecoder =
    map3 Post
        --(field "iserId" int)
        (field "id" int)
        (field "title" string)
        (field "body" string)



-- VIEW

view : Model -> Browser.Document Msg
view model =
  let
    content = div [class "container"]
        [
          nav []
            [ div [ class "navbar-menu" ] [
            ] ]
          ,div []
            [
                renderBreadcrumbs model
                ,renderBody model
            ]
        ]
    title = getPageTitleText model
  in Browser.Document title [content]


renderBody: Model -> Html msg
renderBody model =
    let
        body = case model.page of
            HomePage -> text "HOME PAGE"
            ErrorPage str -> text ("ERROR: " ++ str)
            ItemPage post -> renderPost post
            ItemsListPage posts -> renderPosts posts
    in
        div [class "content"] [
            body
        ]

renderBreadcrumbs : Model -> Html msg
renderBreadcrumbs model =
    let
        breadcrumbsList =
            ("Posts", Route.Home) :: case model.page of
                ItemPage post -> [(post.title, Route.Item (String.fromInt post.id))]
                _ -> []
        renderItem (title, route) =
                        li [] [
                            a [ href (Route.toString route) ]
                            [ text title ]
                        ]
    in
          nav [ class "breadcrumb is-large" ]
            [
                ul []
                    ( List.map renderItem breadcrumbsList )
            ]

renderPost : Post -> Html msg
renderPost post =
  div [] [
    h1
        [class "title is-1"]
        [ text post.title ]
    ,div
        []
        [ text post.body ]
  ]

renderPosts : List Post -> Html msg
renderPosts posts =
  ul
    []
    (List.map renderPostInList posts)

renderPostInList : Post -> Html msg
renderPostInList post =
  li [] [
    a [
        href (Route.toString (Route.Item (String.fromInt post.id)))
        ,class "is-link"
    ]
    [ text post.title ]
  ]


getPageTitleText : Model -> String
getPageTitleText model =
    case model.page of
        HomePage -> "HOME PAGE"
        ErrorPage str -> "ERROR: " ++ str
        ItemPage post -> post.title
        ItemsListPage posts -> "Posts"




subscriptions : Model -> Sub msg
subscriptions model = Sub.none




-- MAIN
main : Program () Model Msg
main =
  Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }





-- Taken from https://elmprogramming.com/decoding-json-part-2.html
buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


