module SectionParserTest exposing (tests)

import Expect
import Message.Parser as MP
import Parser exposing (Parser)
import Test exposing (..)


runP : Parser a -> String -> Result (List Parser.DeadEnd) a
runP p input =
    Parser.run p input


tests : Test
tests =
    describe "Section parser tests"
        [ describe "preambleP"
            [ test "accepts mbox preamble" <|
                \_ ->
                    "From user@example.com Sun Aug  3 14:14:10 2025\n"
                        |> runP MP.preambleP
                        |> Expect.ok
            , test "rejects body-like From" <|
                \_ ->
                    "From the team\n"
                        |> runP MP.preambleP
                        |> Expect.err
            ]
        , describe "authorP"
            [ test "nested parentheses" <|
                \_ ->
                    "From: addr@example.com (User (Team) Name)\n"
                        |> runP MP.authorP
                        |> Expect.equal (Ok "User (Team) Name")
            ]
        , describe "dateP"
            [ test "valid IMF date" <|
                \_ ->
                    "Date: Sun, 3 Aug 2025 10:14:10 -0400\n"
                        |> runP MP.dateP
                        |> Expect.ok
            ]
        , describe "subjectP and folding"
            [ test "folded subject lines" <|
                \_ ->
                    [ "Subject: Line one"
                    , " continuation"
                    , "\n"
                    ]
                        |> String.join "\n"
                        |> runP MP.subjectP
                        |> Expect.equal (Ok "Line one continuation")
            ]
        , describe "inReplyToP"
            [ test "extract first token" <|
                \_ ->
                    "In-Reply-To: <id@example> (comment)\n"
                        |> runP MP.inReplyToP
                        |> Expect.equal (Ok "<id@example>")
            ]
        , describe "referencesP"
            [ test "simple references" <|
                \_ ->
                    "References: <a> <b>\n"
                        |> runP MP.referencesP
                        |> Expect.equal (Ok "<a> <b>")
            ]
        , describe "messageIdP"
            [ test "angle brackets trimmed" <|
                \_ ->
                    "Message-ID: <id-123>\n"
                        |> runP MP.messageIdP
                        |> Expect.equal (Ok "id-123")
            ]
        , describe "contentP"
            [ test "handles body line starting with From without splitting" <|
                \_ ->
                    let
                        content =
                            String.join "\n"
                                [ "Line"
                                , "From the team"
                                , "End"
                                ]
                    in
                    (content ++ "\n")
                        |> runP MP.contentP
                        |> Expect.equal (Ok (content ++ "\n"))
            ]
        ]
