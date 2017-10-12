module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src)
import Json.Decode as Decode
import Json.Encode as Encode
import Http

---- MODEL ----

type alias Model = User

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( User "No User" "/logo.svg" [], getUserData flags.githubToken )


---- UPDATE ----


type Msg
    = NoOp
    | UserData (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []
    UserData result ->
      case result of
        Ok userResult ->
           ( userResult, Cmd.none )
        Err error ->
          Debug.log (toString error)
          model ! []


---- VIEW ----


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text ("Que pedo, " ++ model.name) ]
    , img [ src model.avatarUrl ] []
    , ul [] ( List.map issuesv model.issues )
    ]

issuesv : Issue -> Html msg
issuesv { name, state, bodyText } =
  li []
    [ h2 [] [ text name ]
    , h3 [] [ text state ]
    , p [] [ text bodyText ]
    ]


--- PROGRAM ----

query : String
query = """
  { viewer
    {
      login
      avatarUrl
      issues(first: 30 states: OPEN) {
        nodes {
          state
          bodyText
          repository { name }
        }
      }
    }
  }
"""

type alias Issue =
  { state : String
  , bodyText : String
  , name : String
  }

type alias User =
    { name : String
    , avatarUrl : String
    , issues : List Issue
    }

decodeIssue : Decode.Decoder (List Issue)
decodeIssue =
  Decode.list (
    Decode.map3 Issue
        (Decode.at ["state"] Decode.string)
        (Decode.at ["bodyText"] Decode.string)
        (Decode.at ["repository", "name"] Decode.string)
  )


decodeUser : Decode.Decoder User
decodeUser =
    Decode.map3 User
        (Decode.at ["data", "viewer", "login"] Decode.string)
        (Decode.at ["data", "viewer", "avatarUrl"] Decode.string)
        (Decode.at ["data", "viewer", "issues", "nodes"] decodeIssue)

requestUser : String -> String -> Http.Request User
requestUser token url =
  let
    headers =
      [ "bearer " ++ token |> Http.header "Authorization" ]
    body =
      [( "query", Encode.string query )]
      |> Encode.object
      |> Encode.encode 0
      |> Http.stringBody "application/json"
  in
    Http.request
      { method = "POST"
      , headers = headers
      , url = url
      , body = body
      , expect = decodeUser |> Http.expectJson
      , timeout = Nothing
      , withCredentials = False
      }

getUserData : String -> Cmd Msg
getUserData token =
  let
    url =
      "https://api.github.com/graphql"
  in
    requestUser token url
    |> Http.send UserData


type alias Flags = { githubToken : String }

main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
