module Pages.Home_ exposing (Model, Msg, page)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes
import Html.Parser
import Http
import Message.Parser
import Page exposing (Page)
import Parser
import Parser.Error
import Route exposing (Route)
import Shared
import View exposing (View)


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
    { messages : Dict String (Maybe (List Message.Parser.Message))
    , problem : Maybe Problem
    }


type Problem
    = HttpError Http.Error
    | HtmlParseError String (List Parser.DeadEnd)
    | MessageParseError String (List Parser.DeadEnd)
    | FailedToFindLinks


init : () -> ( Model, Effect Msg )
init () =
    ( { messages = Dict.empty, problem = Nothing }
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
    | ServerRespondedWithTextFile { href : String } (Result Problem (List Message.Parser.Message))
    | NoOp


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
                        | messages =
                            links
                                |> List.map (\link -> ( link, Nothing ))
                                |> Dict.fromList
                      }
                    , links
                        |> List.take 4
                        |> List.map
                            (\link ->
                                Effect.sendCmd
                                    (Http.get
                                        { url = "/haskell-cafe/" ++ link
                                        , expect =
                                            Http.expectString
                                                (Result.mapError HttpError
                                                    >> Result.andThen
                                                        (\text ->
                                                            Message.Parser.run text
                                                                |> Result.mapError (MessageParseError text)
                                                        )
                                                    >> ServerRespondedWithTextFile { href = link }
                                                )
                                        }
                                    )
                            )
                        |> Effect.batch
                    )

                Result.Err problem ->
                    ( { model | problem = Just problem }, Effect.none )

        ServerRespondedWithTextFile { href } response ->
            case response of
                Result.Ok messages ->
                    ( { model | messages = Dict.insert href (Just messages) model.messages }
                    , Effect.none
                    )

                Result.Err problem ->
                    ( { model | problem = Just problem }, Effect.none )

        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
                Html.ul []
                    (model.messages
                        |> Dict.toList
                        |> List.map
                            (\( link, maybeMessages ) ->
                                case maybeMessages of
                                    Just messages ->
                                        Html.li []
                                            [ Html.div [] [ Html.text link ]
                                            , Html.ul []
                                                (messages
                                                    |> List.map
                                                        (\message ->
                                                            Html.li []
                                                                [ Html.div []
                                                                    [ Html.strong [] [ Html.text "From: " ]
                                                                    , Html.text message.author
                                                                    ]
                                                                , Html.div []
                                                                    [ Html.strong [] [ Html.text "Subject: " ]
                                                                    , Html.text message.subject
                                                                    ]
                                                                , Html.div []
                                                                    [ Html.strong [] [ Html.text "Date: " ]
                                                                    , Html.text (Debug.toString message.date)
                                                                    ]
                                                                , Html.div []
                                                                    [ Html.strong [] [ Html.text "Content: " ]
                                                                    , Html.pre [] [ Html.text message.content ]
                                                                    ]
                                                                ]
                                                        )
                                                )
                                            ]

                                    Nothing ->
                                        Html.li [] [ Html.text link ]
                            )
                    )
        ]
    }


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
