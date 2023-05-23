port module Main exposing (init, main)

import Admin
import Browser
import Browser.Navigation as Nav
import Common exposing (..)
import Dashboard exposing (dashboard)
import File.Download
import GenPage exposing (genPdf)
import Html exposing (div, text)
import Http exposing (Error(..), Response(..), emptyBody)
import Login exposing (submitLogin)
import Mood exposing (mood)
import Name exposing (name)
import Platform.Cmd as Cmd
import PlaylistApi exposing (..)
import Random exposing (..)
import Register exposing (submitRegister)
import Types exposing (..)
import Url exposing (Protocol(..))
import Url.Parser exposing (s, top)


type alias Flags =
    { token : String
    , user : Maybe User
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkCliecked
        }


init :
    Flags
    -> Url.Url
    -> Nav.Key
    -> ( Model, Cmd Msg )
init flags url key =
    let
        route_curr =
            Maybe.withDefault NotFoundRoute (Url.Parser.parse route url)
    in
    ( { counter = 0
      , url = url
      , key = key
      , route =
            route_curr
      , login = initLogin ()
      , register = initRegister ()
      , user = flags.user
      , dashboardUsers = []
      , token = flags.token
      , mood = 5
      , genre = "Rock"
      , name = ""
      , errMsg = ""
      , randomInt = 0
      , divvis = visibleController ()
      , playlist = Nothing
      , playlistBeforeChoosing = []
      , tracks = Nothing
      , playlistsStored = []
      , access_token = "BQCgylLdE3S8q86WFXrv-8oYbROieadICJkFnQGsDC2ts8N0vp-xtQCy1hG4kiWO5WB-Ur95iMtRBv8tXmKOmV1ksZO7UxhoF2YTzytXKDvbmB7OuZZ1"
      , client_id = "26acd433c8d54cccac429ca13f8937da"
      , client_secret = "2a8cf2f59c29437789386aef82a0ea8f"
      , pdfBytes = Nothing
      }
    , case route_curr of
        AdminRoute ->
            Cmd.batch [ Admin.getUsers flags.token ]

        DashboardRoute ->
            Cmd.batch [ Dashboard.get_playlists flags.token ]

        _ ->
            Cmd.none
      -- refreshTokenRequest PlayListSpotifyRequest "26acd433c8d54cccac429ca13f8937da" "2a8cf2f59c29437789386aef82a0ea8f"
    )


route : Url.Parser.Parser (Route -> a) a
route =
    Url.Parser.oneOf
        [ Url.Parser.map HomeRoute top
        , Url.Parser.map NameRoute (s "name")
        , Url.Parser.map AdminRoute (s "admin")
        , Url.Parser.map DashboardRoute (s "dashboard")
        , Url.Parser.map LoginRoute (s "login")
        , Url.Parser.map RegisterRoute (s "register")
        ]


port setStorage : String -> Cmd msg


port getStorage : (String -> msg) -> Sub msg


port removeItem : () -> Cmd msg


errMsg : Http.Error -> Int -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
errMsg e status msg ( model, cmd ) =
    case e of
        BadStatus s ->
            if status == s then
                ( { model | errMsg = msg }, Cmd.none )

            else
                ( model, Cmd.none )

        NetworkError ->
            ( { model | errMsg = msg }, Cmd.none )

        _ ->
            ( model, Cmd.none )


logout : () -> Cmd.Cmd Msg
logout () =
    Http.post
        { url = "http://localhost:3000/login"
        , body = emptyBody
        , expect = Http.expectWhatever LogoutRequest
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkCliecked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                nextRoute =
                    Url.Parser.parse route url
            in
            let
                cmd =
                    Cmd.map (always msg) Cmd.none
            in
            case nextRoute of
                Just r ->
                    case r of
                        AdminRoute ->
                            ( { model | url = url, route = r }, Cmd.batch [ cmd, Admin.getUsers model.token ] )

                        DashboardRoute ->
                            ( { model | url = url, route = r }, Cmd.batch [ cmd, Dashboard.get_playlists model.token ] )

                        _ ->
                            ( { model | url = url, route = r }, cmd )

                Nothing ->
                    ( { model | url = url, route = NotFoundRoute }
                    , cmd
                    )

        LoginUpdate l ->
            ( { model
                | login = l
              }
            , Cmd.none
            )

        LoginSubmit ->
            let
                s =
                    submitLogin model.login
            in
            ( model, s )

        LoginSubmitHttp (Ok ( user, token )) ->
            let
                cmds =
                    [ setStorage token, Nav.pushUrl model.key "/" ]
            in
            -- Debug.log
            --     token
            ( { model | login = initLogin (), user = Just user, token = token, errMsg = "" }, Cmd.batch cmds )

        LoginSubmitHttp (Err e) ->
            errMsg e
                500
                "Login failed!"
                ( model, Cmd.none )

        RegisterUpdate r ->
            ( { model | register = r }, Cmd.none )

        RegisterSubmit ->
            ( model, submitRegister model.register )

        RegisterSubmitHttp (Ok _) ->
            let
                cmds =
                    Nav.pushUrl model.key "/login"
            in
            ( { model | errMsg = "" }, cmds )

        RegisterSubmitHttp (Err e) ->
            errMsg e
                500
                "User already exists!"
                ( model, Cmd.none )

        DashboardUsersList r ->
            case r of
                Ok lu ->
                    ( { model | dashboardUsers = lu, pdfBytes = Just (genPdf lu) }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        UpdateUser id ->
            let
                user =
                    List.filter (\a -> a.id == id) model.dashboardUsers |> List.head
            in
            case user of
                Nothing ->
                    ( model, Cmd.none )

                Just uu ->
                    ( model, Admin.updateUser uu model.token )

        UpdateUserInput users ->
            ( { model | dashboardUsers = users }, Cmd.none )

        UpdateUserSubmit (Ok _) ->
            ( model, Admin.getUsers model.token )

        UpdateUserSubmit (Err _) ->
            ( model, Cmd.none )

        DeleteUser user_id ->
            ( model, Cmd.batch [ Admin.deleteUser user_id model.token ] )

        DeleteUserSubmit ( id, Ok _ ) ->
            ( { model | dashboardUsers = List.filter (\uu -> uu.id /= id) model.dashboardUsers }, Cmd.none )

        DeleteUserSubmit ( _, Err _ ) ->
            ( model, Cmd.none )

        ToggleDiv ->
            ( { model | divvis = { visible1 = not model.divvis.visible1, visible2 = not model.divvis.visible2 } }, Cmd.none )

        MoodUpdate m ->
            ( { model
                | mood = m
              }
            , Cmd.none
            )

        NameUpdate m ->
            ( { model
                | name = m
              }
            , Cmd.none
            )

        GenreUpdate g ->
            ( { model
                | genre = g
              }
            , Cmd.none
            )

        RandomInt x ->
            let
                playlist =
                    nth model.playlistBeforeChoosing x
            in
            let
                m =
                    { model | randomInt = x }
            in
            case playlist of
                Nothing ->
                    Debug.log "playlist empty"
                        ( m, Cmd.none )

                Just play ->
                    ( { m | playlist = Just play }, tracksRequest model.access_token play.tracksHref )

        PlaylistSubmit ->
            ( model, playlistRequest model model.access_token model.mood model.genre )

        PlaylistSubmitName ->
            ( model, playlistRequestName model model.access_token model.name )

        PlaylistRequest (Ok playlist_response) ->
            let
                divv =
                    { visible1 = False, visible2 = False }
            in
            let
                m =
                    { model | playlistBeforeChoosing = playlist_response, divvis = divv }
            in
            let
                cmd =
                    Random.generate RandomInt (Random.int 0 (List.length playlist_response - 1))
            in
            ( m, cmd )

        PlaylistRequest (Err e) ->
            case e of
                BadStatus mdata ->
                    case mdata of
                        401 ->
                            ( model
                            , refreshTokenRequest PlayListSpotifyRequest model.client_id model.client_secret
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RefreshTokenRequest ( repeat, Ok u ) ->
            let
                m =
                    { model | access_token = u.access_token }
            in
            case repeat of
                PlayListSpotifyRequest ->
                    ( m, playlistRequest model model.access_token model.mood model.genre )

                TracksSpotifyRequest ->
                    case model.playlist of
                        Nothing ->
                            ( m, Cmd.none )

                        Just play ->
                            ( m, tracksRequest model.access_token play.tracksHref )

        RefreshTokenRequest ( _, Err _ ) ->
            ( model, Cmd.none )

        TracksRequest (Ok t) ->
            ( { model | tracks = Just t }, Cmd.none )

        TracksRequest (Err e) ->
            case e of
                BadStatus mdata ->
                    case mdata of
                        401 ->
                            ( model
                            , refreshTokenRequest TracksSpotifyRequest model.client_id model.client_secret
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DownloadPdf ->
            case model.pdfBytes of
                Just pdfBytes ->
                    let
                        cmd =
                            File.Download.bytes "Users" "application/pdf" pdfBytes
                    in
                    ( model, cmd )

                Nothing ->
                    ( model, Cmd.none )

        PlaylistStoredRequest (Ok l) ->
            ( { model | playlistsStored = l }, Cmd.none )

        PlaylistStoredRequest (Err _) ->
            ( model, Cmd.none )

        PlaylistStoredRequestDelete ( id, Ok _ ) ->
            ( { model | playlistsStored = List.filter (\a -> a.id /= id) model.playlistsStored }, Cmd.none )

        PlaylistStoredRequestDelete ( _, Err _ ) ->
            ( model, Cmd.none )

        DeletePlaylist id ->
            ( model, Dashboard.delete_playlist id model.token )

        PlaylistStoreSubmit ->
            case model.playlist of
                Nothing ->
                    ( model, Cmd.none )

                Just p ->
                    let
                        url =
                            "https://open.spotify.com/track/" ++ p.id
                    in
                    ( model, Common.storePlaylist p.name url model.token )

        PlaylistStoreRequest _ ->
            ( { model | errMsg = "Success!" }, Cmd.none )

        RestorePlaylist ->
            ( { model | divvis = { visible1 = True, visible2 = False }, playlist = Nothing, tracks = Nothing, genre = "Rock", mood = 5 }, Cmd.none )

        LogoutSubmit ->
            let
                cmd =
                    Cmd.batch [ removeItem (), logout (), Nav.pushUrl model.key "/" ]
            in
            ( { model | user = Nothing, token = "" }, cmd )

        LogoutRequest _ ->
            ( model, Cmd.none )



-- ( model, refreshTokenRequest TracksSpotifyRequest model.client_id model.client_secret )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Soundscapes"
    , body =
        [ case model.route of
            HomeRoute ->
                mood model

            NameRoute ->
                name model

            AdminRoute ->
                let
                    not_found =
                        div [] [ text "Not Found!" ]
                in
                case model.user of
                    Nothing ->
                        not_found

                    Just user ->
                        if user.role == 1 then
                            Admin.admin model

                        else
                            not_found

            LoginRoute ->
                Login.login model

            RegisterRoute ->
                Register.register model

            NotFoundRoute ->
                div [] [ text "Not Found!" ]

            DashboardRoute ->
                dashboard model
        ]
    }
