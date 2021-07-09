module Main exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, src, style)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..))
import Url exposing (Url)


type alias Model =
    { key : Key
    , user : RemoteData Http.Error User
    , repos : RemoteData Http.Error (List Repo)
    }


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | GotUser (RemoteData Http.Error User)
    | GotRepos (RemoteData Http.Error (List Repo))


type alias User =
    { name : String
    , avatarUrl : String
    , bio : String
    , location : String
    , url : String
    , blogUrl : String
    }


type alias Repo =
    { name : String
    , description : Maybe String
    , hasPages : Bool
    , fork : Bool
    , stars : Int
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
init _ _ key =
    let
        username =
            "Herteby"
    in
    ( { key = key
      , user = Loading
      , repos = Loading
      }
    , Cmd.batch
        [ Http.get
            { url = "https://api.github.com/users/" ++ username
            , expect = Http.expectJson (RemoteData.fromResult >> GotUser) userDecoder
            }
        , Http.get
            { url = "https://api.github.com/users/" ++ username ++ "/repos?per_page=100"
            , expect = Http.expectJson (RemoteData.fromResult >> GotRepos) (Decode.list repoDecoder)
            }
        ]
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

        GotUser remoteData ->
            ( { model | user = remoteData }, Cmd.none )

        GotRepos remoteData ->
            ( { model | repos = remoteData }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Simon Herteby"
    , body =
        [ case RemoteData.map2 Tuple.pair model.user model.repos of
            NotAsked ->
                none

            Loading ->
                h1 [] [ text "Loading" ]

            Failure error ->
                viewError error

            Success ( user, repos ) ->
                let
                    ( withPages, other ) =
                        repos
                            |> List.filter (not << .fork)
                            |> List.sortBy (negate << .stars)
                            |> List.partition .hasPages
                in
                divClass "page"
                    [ divClass "profile"
                        [ img [ src user.avatarUrl, class "avatar" ] []
                        , divClass "stuff"
                            [ a [ href user.url ] [ h1 [] [ text user.name ] ]
                            , div [] [ text user.bio ]
                            , div [] [ text user.location ]
                            ]
                        ]
                    , divClass "repos"
                        (h2 [] [ text "Pages" ]
                            :: (withPages
                                    |> List.map
                                        (\repo ->
                                            divClass "repo"
                                                [ h3 [ class "repoName" ] [ a [ href ("https://" ++ user.blogUrl ++ "/" ++ repo.name) ] [ text repo.name ] ]
                                                , case repo.description of
                                                    Just description ->
                                                        div [ class "description" ] [ text description ]

                                                    Nothing ->
                                                        none
                                                ]
                                        )
                               )
                        )
                    , divClass "repos"
                        (h2 [] [ text "Repos" ]
                            :: (other
                                    |> List.map
                                        (\repo ->
                                            divClass "repo"
                                                [ h3 [ class "repoName" ] [ a [ href (user.url ++ "/" ++ repo.name) ] [ text repo.name ] ]
                                                , case repo.description of
                                                    Just description ->
                                                        div [ class "description" ] [ text description ]

                                                    Nothing ->
                                                        none
                                                ]
                                        )
                               )
                        )
                    ]
        ]
    }


divClass className =
    div [ class className ]


none : Html msg
none =
    text ""


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


userDecoder : Decoder User
userDecoder =
    Decode.map6 User
        (Decode.field "name" Decode.string)
        (Decode.field "avatar_url" Decode.string)
        (Decode.field "bio" Decode.string)
        (Decode.field "location" Decode.string)
        (Decode.field "html_url" Decode.string)
        (Decode.field "blog" Decode.string)


repoDecoder : Decoder Repo
repoDecoder =
    Decode.map5 Repo
        (Decode.field "name" Decode.string)
        (Decode.field "description" (Decode.nullable Decode.string))
        (Decode.field "has_pages" Decode.bool)
        (Decode.field "fork" Decode.bool)
        (Decode.field "stargazers_count" Decode.int)
