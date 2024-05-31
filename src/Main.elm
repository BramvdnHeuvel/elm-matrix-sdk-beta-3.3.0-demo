module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Matrix exposing (Vault, VaultUpdate)
import Json.Encode as E
import Html.Attributes exposing (type_)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Html.Attributes exposing (placeholder)
import Html.Attributes exposing (style)
import Matrix.Settings

-- MODEL

type Model
    = Authenticating
        { accessToken : String
        , password : String
        , roomId : String
        , username : String
        }
    | Sending { roomId : String, text : String, transactionId : Int, vault : Vault }


initialModel : ( Model, Cmd Msg )
initialModel =
    ( Authenticating
        { accessToken = "", password = "", roomId = "", username = "" }
    , Cmd.none
    )


type Msg
    = OnReceive VaultUpdate
    | SetAccessToken String
    | SetPassword String
    | SetRoomId String
    | SetText String
    | SetUsername String
    | SendMessage
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model) of
        ( OnReceive vu, Sending data ) ->
            ( Sending { data | vault = Matrix.update vu data.vault }
            , Cmd.none
            )
        
        ( SetAccessToken t, Authenticating data ) ->
            ( Authenticating { data | accessToken = t }
            , Cmd.none
            )
        
        ( SetPassword p, Authenticating data ) ->
            ( Authenticating { data | password = p }
            , Cmd.none
            )
        
        ( SetRoomId r, Authenticating data ) ->
            ( Authenticating { data | roomId = r }
            , Cmd.none
            )
        
        ( SetText t, Sending data ) ->
            ( Sending { data | text = t }
            , Cmd.none
            )
        
        ( SetUsername u, Authenticating data ) ->
            ( Authenticating { data | username = u }
            , Cmd.none
            )

        ( SendMessage, Sending data ) ->
            ( Sending { data | transactionId = data.transactionId + 1 }
            , Matrix.sendMessageEvent
                { content =
                    E.object
                        [ ( "body", E.string data.text )
                        , ( "msgtype", E.string "m.text" )
                        ]
                , eventType = "m.room.message"
                , roomId = data.roomId
                , toMsg = OnReceive
                , transactionId =
                    "elm-matrix-sdk-beta-3.3.0-" ++ (String.fromInt data.transactionId)
                , vault = data.vault
                }
            )
        
        ( Submit, Authenticating data ) ->
            case canSubmit model of
                Nothing ->
                    ( model, Cmd.none )
                
                Just v ->
                    ( Sending
                        { roomId = data.roomId
                        , text = ""
                        , transactionId = 0
                        , vault = v
                        }
                    , Cmd.none
                    )
        
        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Elm SDK demo"
    , body =
        case model of
            Authenticating data ->
                div
                    [ style "display" "flex"
                    , style "font-family" "monospace"
                    , style "justify-content" "center"
                    , style "flex-direction" "column"
                    , style "padding" "20px"
                    ]
                    [ h3 [] [ text "Username" ]
                    , showInput
                        { onInput = SetUsername
                        , placeholder = "@alice:example.com"
                        , value = data.username
                        }

                    , h3 [] [ text "Credentials" ]
                    , p [] [ text "Only one of the two is required." ]
                    , h4 [] [ text "Password" ]
                    , input
                        [ onInput SetPassword
                        , type_ "password"
                        , value data.password
                        ]
                        []
                    , h4 [] [ text "Access token" ]
                    , input
                        [ onInput SetAccessToken
                        , type_ "password"
                        , value data.accessToken
                        ]
                        []
                    
                    , h3 [] [ text "Room ID" ]
                    , showInput
                        { onInput = SetRoomId
                        , placeholder = "!abcdef:example.com"
                        , value = data.roomId
                        }
                    
                    , case canSubmit model of
                        Just _ ->
                            button [ onClick Submit ] [ text "LOG IN" ]
                        
                        Nothing ->
                            div [] []
                    ]
                    |> List.singleton
            
            Sending data ->
                div
                    [ style "display" "flex"
                    , style "font-family" "monospace"
                    , style "justify-content" "center"
                    , style "flex-direction" "column"
                    , style "padding" "20px"
                    ]
                    [ showInput
                        { onInput = SetText
                        , placeholder = "hello, world!"
                        , value = data.text
                        }
                    
                    , button [ onClick SendMessage ] [ text "SEND" ]
                    ]
                    |> List.singleton
    }

canSubmit : Model -> Maybe Vault
canSubmit model =
    case model of
        Sending _ ->
            Nothing
        
        Authenticating data ->
            data.username
                |> Matrix.fromUserId
                |> Maybe.andThen
                    (\v ->
                        case (data.accessToken, data.password) of
                            ("", "") ->
                                Nothing
                            
                            ("", p) ->
                                Just <| Matrix.Settings.setPassword p v
                            
                            (a, _) ->
                                Just <| Matrix.Settings.setAccessToken a v
                            
                    )
                |> Maybe.andThen
                    ( if data.roomId == "" then
                        always Nothing
                    else
                        Maybe.Just
                    )

showInput : { onInput : String -> msg, placeholder : String, value : String } -> Html msg
showInput data =
    input
        [ onInput data.onInput
        , placeholder data.placeholder
        , type_ "text"
        , value data.value
        ]
        []

main : Program () Model Msg
main =
    Browser.document
        { init = always initialModel
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
