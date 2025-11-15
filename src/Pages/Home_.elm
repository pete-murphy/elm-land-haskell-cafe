module Pages.Home_ exposing (Model, Msg, page)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Html
import Html.Parser
import Html.Parser.Util
import Http
import Page exposing (Page)
import Parser
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
    { links : Dict String (Maybe Html.Parser.Document)
    }


type Problem
    = HttpError Http.Error
    | HtmlParseError (List Parser.DeadEnd)
    | FailedToFindLinks


init : () -> ( Model, Effect Msg )
init () =
    ( { links = Dict.empty }
    , Effect.sendCmd
        (Http.get
            { url = "/haskell-cafe/index.html"
            , expect =
                Http.expectString
                    (Result.mapError HttpError
                        >> Result.andThen
                            (Html.Parser.runDocument >> Result.mapError HtmlParseError)
                        >> Result.andThen
                            (\doc ->
                                case findLinks doc of
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


findLinks : Html.Parser.Document -> List String
findLinks parserDocument =
    case parserDocument.document of
        ( _, nodes ) ->
            let
                go acc nodes_ =
                    case nodes_ of
                        [] ->
                            acc

                        node :: rest ->
                            case node of
                                Html.Parser.Element "a" attributes children ->
                                    case children of
                                        [ Html.Parser.Text "[ Date ]" ] ->
                                            let
                                                maybeHref =
                                                    attributes
                                                        |> List.filterMap
                                                            (\( name, value ) ->
                                                                if name == "href" then
                                                                    Just value

                                                                else
                                                                    Nothing
                                                            )
                                                        |> List.head
                                            in
                                            case maybeHref of
                                                Just href ->
                                                    go (href :: acc) rest

                                                Nothing ->
                                                    go acc rest

                                        _ ->
                                            go acc rest

                                Html.Parser.Element _ _ children ->
                                    go acc children
                                        ++ go acc rest

                                Html.Parser.Text _ ->
                                    go acc rest

                                Html.Parser.Comment _ ->
                                    go acc rest
            in
            go [] nodes



-- UPDATE


type Msg
    = ServerRespondedWithIndexPage (Result Problem (List String))
    | ServerRespondedWithPostsByMonth { href : String } (Result Problem Html.Parser.Document)
    | NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ServerRespondedWithIndexPage response ->
            case response of
                Result.Ok links ->
                    ( { model
                        | links =
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
                                                        (Html.Parser.runDocument
                                                            >> Result.mapError HtmlParseError
                                                        )
                                                    >> ServerRespondedWithPostsByMonth { href = link }
                                                )
                                        }
                                    )
                            )
                        |> Effect.batch
                    )

                Result.Err problem ->
                    ( model, Effect.none )

        ServerRespondedWithPostsByMonth { href } response ->
            case response of
                Result.Ok parserDocument ->
                    ( { model | links = Dict.insert href (Just parserDocument) model.links }
                    , Effect.none
                    )

                Result.Err problem ->
                    ( model, Effect.none )

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
        [ Html.ul []
            (model.links
                |> Dict.toList
                |> List.map
                    (\( link, maybeDocument ) ->
                        case maybeDocument of
                            Just parserDocument ->
                                Html.li []
                                    [ Html.div [] [ Html.text link ]
                                    , Html.div []
                                        (Tuple.second parserDocument.document
                                            |> Html.Parser.Util.toVirtualDom
                                        )
                                    ]

                            Nothing ->
                                Html.li [] [ Html.text link ]
                    )
            )
        ]
    }
