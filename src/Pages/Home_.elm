port module Pages.Home_ exposing (Model, Msg, page)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes
import Html.Parser
import Http
import Iso8601
import Json.Decode
import Json.Encode
import Message.Parser
import Page exposing (Page)
import Parser
import Parser.Error
import Route exposing (Route)
import Shared
import View exposing (View)



-- Ports


port toBackend : Json.Encode.Value -> Cmd msg


port fromBackend : (Json.Decode.Value -> msg) -> Sub msg


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { files : Dict String FileStatus
    , problem : Maybe Problem
    }


type alias FileStatus =
    { state : FileState
    , total : Int
    , inserted : Int
    }


type FileState
    = Queued
    | Fetching
    | Parsing
    | Storing
    | Done
    | Error String


type Problem
    = HttpError Http.Error
    | HtmlParseError String (List Parser.DeadEnd)
    | MessageParseError String (List Parser.DeadEnd)
    | FailedToFindLinks


init : () -> ( Model, Effect Msg )
init () =
    ( { files = Dict.empty, problem = Nothing }
    , Effect.sendCmd
        (Http.get
            { url = "/haskell-cafe/index.html"
            , expect =
                Http.expectString
                    (Result.mapError HttpError
                        >> Result.andThen parseHtml4Document
                        >> Result.andThen
                            (\doc ->
                                let
                                    res =
                                        doc
                                            |> findLinksMatching
                                                (\{ attributes, children } ->
                                                    case children of
                                                        [ Html.Parser.Text text ] ->
                                                            if String.startsWith "[ Text " text then
                                                                attributes
                                                                    |> List.filterMap
                                                                        (\( name, value ) ->
                                                                            if name == "href" then
                                                                                Just value

                                                                            else
                                                                                Nothing
                                                                        )
                                                                    |> List.head

                                                            else
                                                                Nothing

                                                        _ ->
                                                            Nothing
                                                )
                                in
                                case res of
                                    [] ->
                                        Result.Err FailedToFindLinks

                                    links ->
                                        Result.Ok links
                            )
                        >> ServerRespondedWithIndexPage
                    )
            }
        )
    )


findLinksMatching :
    ({ attributes : List ( String, String ), children : List Html.Parser.Node } -> Maybe x)
    -> Html.Parser.Document
    -> List x
findLinksMatching f parserDocument =
    let
        go acc nodes =
            case nodes of
                [] ->
                    acc

                node :: rest ->
                    case node of
                        Html.Parser.Element "a" attributes children ->
                            case f { attributes = attributes, children = children } of
                                Just href ->
                                    go (href :: acc) rest

                                Nothing ->
                                    go acc rest

                        Html.Parser.Element _ _ children ->
                            go acc children
                                ++ go acc rest

                        Html.Parser.Text _ ->
                            go acc rest

                        Html.Parser.Comment _ ->
                            go acc rest
    in
    go [] [ parserDocument.root ]



-- UPDATE


type Msg
    = ServerRespondedWithIndexPage (Result Problem (List String))
    | ServerRespondedWithTextFile { href : String, index : Int } (Result Problem (List Message.Parser.Message))
    | BackendProgress ProgressMsg
    | NoOp


type alias ProgressMsg =
    { kind : String
    , file : String
    , inserted : Maybe Int
    , total : Maybe Int
    , error : Maybe String
    }


parseHtml4Document : String -> Result Problem Html.Parser.Document
parseHtml4Document html4 =
    let
        str =
            String.lines html4
                |> List.drop 1
                |> List.append [ "<!DOCTYPE html>" ]
                |> String.join "\n"
    in
    Html.Parser.runDocument Html.Parser.noCharRefs str
        |> Result.mapError (HtmlParseError str)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ServerRespondedWithIndexPage response ->
            case response of
                Result.Ok links ->
                    ( { model
                        | files =
                            links
                                |> List.foldl (\href acc -> Dict.insert href { state = Queued, total = 0, inserted = 0 } acc) Dict.empty
                      }
                    , links
                        |> List.indexedMap
                            (\index href ->
                                Effect.sendCmd
                                    (Http.get
                                        { url = "/haskell-cafe/" ++ href
                                        , expect =
                                            Http.expectString
                                                (Result.mapError HttpError
                                                    >> Result.andThen
                                                        (\text ->
                                                            Message.Parser.run text
                                                                |> Result.mapError (MessageParseError text)
                                                        )
                                                    >> ServerRespondedWithTextFile { href = href, index = index }
                                                )
                                        }
                                    )
                            )
                        |> Effect.batch
                    )

                Result.Err problem ->
                    ( { model | problem = Just problem }, Effect.none )

        ServerRespondedWithTextFile { href, index } response ->
            case response of
                Result.Ok messages ->
                    let
                        total =
                            List.length messages

                        payload =
                            Json.Encode.object
                                [ ( "type", Json.Encode.string "upsertMessages" )
                                , ( "file", Json.Encode.string href )
                                , ( "items", Json.Encode.list identity (List.map (encodeMessage href) messages) )
                                ]
                    in
                    ( { model
                        | files =
                            Dict.update href
                                (\_ -> Just { state = Storing, total = total, inserted = 0 })
                                model.files
                      }
                    , Effect.sendCmd (toBackend payload)
                    )

                Result.Err problem ->
                    ( { model
                        | files =
                            Dict.update href
                                (\_ -> Just { state = Error "parse", total = 0, inserted = 0 })
                                model.files
                        , problem = Just problem
                      }
                    , Effect.none
                    )

        NoOp ->
            ( model
            , Effect.none
            )

        BackendProgress pm ->
            case pm.kind of
                "progress" ->
                    let
                        inserted =
                            Maybe.withDefault 0 pm.inserted

                        total =
                            Maybe.withDefault 0 pm.total
                    in
                    ( { model
                        | files =
                            Dict.update pm.file
                                (\maybe ->
                                    case maybe of
                                        Just s ->
                                            Just { s | state = Storing, inserted = inserted, total = max s.total total }

                                        Nothing ->
                                            Just { state = Storing, inserted = inserted, total = total }
                                )
                                model.files
                      }
                    , Effect.none
                    )

                "done" ->
                    ( { model
                        | files =
                            Dict.update pm.file
                                (\maybe ->
                                    case maybe of
                                        Just s ->
                                            Just { s | state = Done }

                                        Nothing ->
                                            Just { state = Done, inserted = 0, total = 0 }
                                )
                                model.files
                      }
                    , Effect.none
                    )

                "error" ->
                    ( { model
                        | files =
                            Dict.update pm.file
                                (\_ -> Just { state = Error (Maybe.withDefault "unknown" pm.error), inserted = 0, total = 0 })
                                model.files
                      }
                    , Effect.none
                    )

                "backpressure" ->
                    -- For now, just keep UI as Storing; a future enhancement could pause fetches
                    ( model, Effect.none )

                _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    fromBackend decodeProgress



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Home_"
    , body =
        [ case model.problem of
            Just problem ->
                case problem of
                    HttpError error ->
                        Html.text (Debug.toString error)

                    HtmlParseError src deadEnds ->
                        errorToHtml src deadEnds

                    MessageParseError src deadEnds ->
                        errorToHtml src deadEnds

                    FailedToFindLinks ->
                        Html.text "Failed to find links"

            Nothing ->
                model.files
                    |> Dict.toList
                    |> List.map
                        (\( href, status ) ->
                            Html.li []
                                [ Html.div [] [ Html.text href ]
                                , renderStatus status
                                ]
                        )
                    |> (\items -> Html.ul [] items)
        ]
    }


renderStatus : FileStatus -> Html msg
renderStatus s =
    case s.state of
        Queued ->
            Html.text "Queued"

        Fetching ->
            Html.text "Fetching…"

        Parsing ->
            Html.text "Parsing…"

        Storing ->
            Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "gap" "8px"
                ]
                [ progressBar s.inserted s.total
                , let
                    pct =
                        if s.total <= 0 then
                            0

                        else
                            round ((toFloat s.inserted / toFloat s.total) * 100)
                  in
                  Html.text
                    (String.fromInt s.inserted
                        ++ "/"
                        ++ String.fromInt s.total
                        ++ " ("
                        ++ String.fromInt pct
                        ++ "%)"
                    )
                ]

        Done ->
            Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "gap" "8px"
                ]
                [ progressBar 1 1
                , Html.text "Done"
                ]

        Error e ->
            Html.text ("Error: " ++ e)


progressBar : Int -> Int -> Html msg
progressBar inserted total =
    let
        maxVal =
            max 1 total
    in
    Html.node "progress"
        [ Html.Attributes.attribute "value" (String.fromInt inserted)
        , Html.Attributes.attribute "max" (String.fromInt maxVal)
        , Html.Attributes.style "width" "480px"
        ]
        []


errorToHtml :
    String
    -> List (Parser.Error.DeadEnd {} Parser.Problem)
    -> Html msg
errorToHtml src deadEnds =
    let
        color : String -> Html msg -> Html msg
        color value child =
            Html.span [ Html.Attributes.style "color" value ] [ child ]
    in
    Parser.Error.renderError
        { text = Html.text
        , formatContext = color "cyan"
        , formatCaret = color "red"
        , linesOfExtraContext = 3
        , newline = Html.br [] []
        }
        Parser.Error.forParser
        src
        deadEnds
        |> Html.pre []



-- ENCODERS/DECODERS


encodeMessage : String -> Message.Parser.Message -> Json.Encode.Value
encodeMessage monthHref msg =
    Json.Encode.object
        [ ( "id", Json.Encode.string msg.messageId )
        , ( "subject", Json.Encode.string msg.subject )
        , ( "fromAddr", Json.Encode.string msg.author )
        , ( "dateIso", Json.Encode.string (Iso8601.fromTime msg.date) )
        , ( "inReplyTo", maybeString msg.inReplyTo )
        , ( "references", maybeRefs msg.references )
        , ( "content", Json.Encode.string msg.content )
        , ( "monthFile", Json.Encode.string monthHref )
        ]


maybeString : Maybe String -> Json.Encode.Value
maybeString m =
    case m of
        Just s ->
            Json.Encode.string s

        Nothing ->
            Json.Encode.null


maybeRefs : Maybe String -> Json.Encode.Value
maybeRefs m =
    case m of
        Just s ->
            Json.Encode.list Json.Encode.string
                (s
                    |> String.split " "
                    |> List.filter (\x -> x /= "")
                )

        Nothing ->
            Json.Encode.null


progressDecoder : Json.Decode.Decoder ProgressMsg
progressDecoder =
    Json.Decode.map5 ProgressMsg
        (Json.Decode.field "type" Json.Decode.string)
        (Json.Decode.field "file" Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.field "inserted" Json.Decode.int))
        (Json.Decode.maybe (Json.Decode.field "total" Json.Decode.int))
        (Json.Decode.maybe (Json.Decode.field "error" Json.Decode.string))


decodeProgress : Json.Decode.Value -> Msg
decodeProgress val =
    case Json.Decode.decodeValue progressDecoder val of
        Ok p ->
            BackendProgress p

        Err _ ->
            NoOp
