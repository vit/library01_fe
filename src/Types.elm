module Types exposing (..)

import Browser
import Browser.Navigation as Nav
import Url
import Http

import Route exposing (..)

-- MODEL

type Page
    = HomePage
    | ErrorPage String
    | ItemPage Post
    --| ItemsListPage String
    | ItemsListPage (List Post)

type alias Model = {
        key: Nav.Key,
        url: Url.Url,
        --maybeRoute: Maybe Route,
        page: Page
    }

type alias Post = {
        --userId: Int,
        id: Int,
        title: String,
        body: String
    }



-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  --| ItemDataReceived (Result Http.Error String)
  --| ItemDataReceived (Result Http.Error (List Post))
  | ItemDataReceived (Result Http.Error Post)
  | ItemsListDataReceived (Result Http.Error (List Post))

