module Main exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, style)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..))
import Url exposing (Url)


type alias Model =
    { key : Key
    , repos : RemoteData Http.Error (List Repo)
    }


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | GotRepos (RemoteData Http.Error (List Repo))


type alias Repo =
    { name : String
    , description : Maybe String
    , hasPages : Bool
    , fork : Bool
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        , subscriptions = subscriptions
        }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init stored url key =
    ( { key = key
      , repos = Loading
      }
    , Http.get
        { url = "https://api.github.com/users/Herteby/repos"
        , expect = Http.expectJson (RemoteData.fromResult >> GotRepos) (Decode.list repoDecoder)
        }
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlChange url ->
            ( model, Cmd.none )

        OnUrlRequest (Internal url) ->
            ( model
            , Nav.load (Url.toString url)
            )

        OnUrlRequest (External url) ->
            ( model
            , Nav.load url
            )

        GotRepos remoteData ->
            ( { model | repos = remoteData }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Simon Herteby"
    , body =
        [ case model.repos of
            NotAsked ->
                none

            Loading ->
                h1 [] [ text "Loading" ]

            Failure error ->
                viewError error

            Success repos ->
                column 20
                    []
                    [ h1 [] [ text "Simon Herteby" ]
                    , column 20
                        []
                        [ h2 [] [ text "Pages" ]
                        , repos
                            |> List.filter (\repo -> repo.hasPages && not repo.fork)
                            |> List.map
                                (\repo ->
                                    a [ href ("https://herteby.github.io/" ++ repo.name) ]
                                        [ row 10
                                            [ class "repo" ]
                                            [ h3 [ class "repoName" ] [ text repo.name ]
                                            , case repo.description of
                                                Just description ->
                                                    div [ class "description" ] [ text description ]

                                                Nothing ->
                                                    none
                                            ]
                                        ]
                                )
                            |> column 20 []
                        ]
                    ]
        ]
    }


none : Html msg
none =
    text ""


row spacing attrs =
    div <| [ class "row", attribute "style" ("--spacing:" ++ String.fromInt spacing ++ "px") ] ++ attrs


column spacing attrs =
    div <| [ class "column", attribute "style" ("--spacing:" ++ String.fromInt spacing ++ "px") ] ++ attrs


viewError : Http.Error -> Html msg
viewError error =
    pre []
        [ text <|
            case error of
                BadUrl string ->
                    "Bad url: " ++ string

                Timeout ->
                    "Timeout"

                NetworkError ->
                    "Network error"

                BadStatus int ->
                    "Bad status: " ++ String.fromInt int

                BadBody string ->
                    string
        ]


repoDecoder : Decoder Repo
repoDecoder =
    Decode.map4 Repo
        (Decode.field "name" Decode.string)
        (Decode.field "description" (Decode.nullable Decode.string))
        (Decode.field "has_pages" Decode.bool)
        (Decode.field "fork" Decode.bool)
