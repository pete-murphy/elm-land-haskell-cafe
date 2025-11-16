module Message.Parser exposing
    ( Header
    , Message
    , authorP
    , contentP
    , dateP
    , headerP
    , inReplyToP
    , messageIdP
    , messageP
    , messagesP
    , preambleP
    , referencesP
    , run
    , runWithDebug
    , subjectP
    )

import Imf.DateTime
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , backtrackable
        , chompWhile
        , end
        , getChompedString
        , loop
        , map
        , oneOf
        , problem
        , succeed
        , symbol
        )
import Time exposing (Posix)


{-| Enable/disable in-source parser debugging. Set to True while diagnosing,
but keep False by default to avoid console noise in production.
-}
debugParser : Bool
debugParser =
    False


{-| Conditional debug log that is a no-op when `debugParser = False`.
-}
dbg : String -> a -> a
dbg tag value =
    if debugParser then
        Debug.log tag value

    else
        value


{-| Simple helper for one-off breadcrumbs.
-}
debugValue : String -> a -> a
debugValue =
    dbg


{-| Trace wrapper around a parser, logging entry/exit with a short label.
Note: We do not log offsets to avoid depending on Parser.Advanced.
-}
traceOffset : String -> Parser a -> Parser a
traceOffset label parser_ =
    dbg (label ++ " before") ()
        |> (\_ ->
                parser_
                    |> andThen
                        (\result ->
                            dbg (label ++ " after") ()
                                |> (\_ -> succeed result)
                        )
           )


{-| Trace a loop step function by logging before and after each iteration.
-}
traceLoop : String -> (s -> Parser (Step s a)) -> (s -> Parser (Step s a))
traceLoop label step =
    \state ->
        dbg (label ++ " iter start") ()
            |> (\_ ->
                    step state
                        |> andThen
                            (\outcome ->
                                dbg (label ++ " iter end") ()
                                    |> (\_ -> succeed outcome)
                            )
               )


{-| Run the top-level messages parser.
-}
run : String -> Result (List Parser.DeadEnd) (List Message)
run =
    Parser.run (traceOffset "messagesP" messagesP)


{-| Same as `run`, but logs a concise preview and the parser dead-ends on failure.
This is helpful for harvesting failing examples from DevTools.
-}
runWithDebug : String -> Result (List Parser.DeadEnd) (List Message)
runWithDebug input =
    case Parser.run messagesP input of
        Ok value ->
            Ok value

        Err deadEnds ->
            let
                preview =
                    String.left 200 input
            in
            Debug.log "Parse failure" ( preview, deadEnds )
                |> (\_ -> Err deadEnds)


{-| Parse a sequence of mbox messages.
-}
messagesP : Parser (List Message)
messagesP =
    loop [] (traceLoop "collectMessages" collectMessages)


collectMessages : List Message -> Parser (Step (List Message) (List Message))
collectMessages acc =
    oneOf
        [ succeed identity
            |= messageP
            |> andThen
                (\msg ->
                    let
                        _ =
                            dbg "collectMessages:parsed" { accLen = List.length acc + 1, subject = msg.subject }
                    in
                    oneOf
                        [ succeed (Done (List.reverse (msg :: acc)))
                            |. end
                        , succeed (Loop (msg :: acc))
                        ]
                )
        , succeed (Done (List.reverse acc))
            |. chompWhile (\c -> c == ' ' || c == '\n' || c == '\t' || c == '\u{000D}')
            |. end
        ]



-- Types
{- Header fields collected from the mbox headers. -}


type alias Header =
    { author : String
    , date : Posix
    , subject : String
    , inReplyTo : Maybe String
    , references : Maybe String
    , messageId : String
    }


{-| Parsed message with normalized content and header data.
-}
type alias Message =
    { content : String
    , author : String
    , subject : String
    , inReplyTo : Maybe String
    , references : Maybe String
    , messageId : String
    , date : Posix
    }



-- Helper functions


{-| Whitespace used when detecting header folding lines.
-}
isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t'


{-| Capture characters while a predicate holds.
-}
takeWhile : (Char -> Bool) -> Parser String
takeWhile predicate =
    getChompedString (chompWhile predicate)


{-| Heuristic check for an mbox-style preamble line that starts a new message.

We intentionally avoid treating arbitrary body lines that start with "From "
as new messages. A preamble typically looks like:

       From user@example.com Sun Aug  3 14:14:10 2025

Heuristics:

  - must start with "From "
  - must contain an '@' (email-like)
  - must contain a digit (day/time)

-}
isMboxPreamble : String -> Bool
isMboxPreamble line =
    let
        hasEmailish =
            String.any (\c -> c == '@') line
                || String.contains " at " line
    in
    String.startsWith "From " line
        && hasEmailish
        && String.any Char.isDigit line



-- Parse timestamp string in format "%e %b %Y %H:%M:%S %z"
-- This is a simplified version - you may need to enhance this
-- parseTimestamp : String -> Maybe Posix
-- parseTimestamp _ =
--     -- Drop first 4 characters (e.g., "Date")
--     -- For now, return Nothing - you'll need to implement actual parsing
--     -- or use a date parsing library. Elm's Time module requires Posix time,
--     -- so you'd need to parse the date string and convert to Posix
--     -- This is a placeholder that maintains the structure
--     Imf.DateTime.p
-- Parsers


{-| Parse the mbox preamble line.
-}
preambleP : Parser ()
preambleP =
    succeed identity
        |= (symbol "From "
                |> andThen
                    (\_ ->
                        getChompedString (chompWhile (\c -> c /= '\n'))
                            |> map (\rest -> "From " ++ rest)
                    )
           )
        |. symbol "\n"
        |> andThen
            (\line ->
                if isMboxPreamble line then
                    dbg "preamble:accepted" (String.left 80 line)
                        |> (\_ -> succeed ())

                else
                    dbg "preamble:rejected" (String.left 80 line)
                        |> (\_ -> problem "Not a preamble")
            )


{-| Parse the author address and display name from the `From:` header,
supporting nested parentheses in the display name.
-}
authorP : Parser String
authorP =
    succeed identity
        |. symbol "From: "
        |. takeWhile (\c -> c /= '(')
        |= betweenParensBalanced
        |. symbol "\n"


{-| Parse a balanced parenthesis expression and return its inner text.
Allows nested parentheses like `(John (Team) Doe)`.
-}
betweenParensBalanced : Parser String
betweenParensBalanced =
    succeed identity
        |. symbol "("
        |= balancedContent
        |. symbol ")"


balancedContent : Parser String
balancedContent =
    loop [] (traceLoop "balancedContent" balancedContentLoop)
        |> map (\parts -> parts |> List.reverse |> String.concat)


balancedContentLoop : List String -> Parser (Step (List String) (List String))
balancedContentLoop acc =
    oneOf
        [ -- End of this balanced segment (caller consumes ')')
          backtrackable (symbol ")" |> andThen (\_ -> problem "end"))
            |> andThen (\_ -> succeed (Done acc))
        , -- Nested '(' ... ')'
          backtrackable
            (symbol "("
                |> andThen
                    (\_ ->
                        balancedContent
                            |. symbol ")"
                            |> map (\inner -> "(" ++ inner ++ ")")
                    )
            )
            |> andThen (\inner -> succeed (Loop (inner :: acc)))
        , -- Plain text chunk without parentheses
          getChompedString (chompWhile (\c -> c /= '(' && c /= ')'))
            |> andThen
                (\txt ->
                    if String.isEmpty txt then
                        -- Potential no-progress loop; log for diagnostics
                        dbg "balanced:empty-chunk" ()
                            |> (\_ -> problem "balanced:no-progress")

                    else
                        succeed (Loop (txt :: acc))
                )
        , succeed (Done acc)
        ]


{-| Consume a remainder of a header value, allowing folded lines.
-}
singleLineRemainder : Parser String
singleLineRemainder =
    traceOffset "singleLineRemainder"
        (oneOf
            [ succeed identity
                |= getChompedString (chompWhile (\c -> c /= '\n'))
                |. symbol "\n"
            , succeed identity
                |= getChompedString (chompWhile (\_ -> True))
                |. end
            ]
        )


{-| Parse possibly folded header value, joining with spaces.
-}
lineRemainderP : Parser String
lineRemainderP =
    traceOffset "lineRemainderP"
        (succeed (::)
            |= singleLineRemainder
            |= loop [] collectLineRemainders
            |> map (List.map String.trim >> String.join " ")
        )


collectLineRemainders : List String -> Parser (Step (List String) (List String))
collectLineRemainders acc =
    oneOf
        [ succeed (Done (List.reverse acc))
            |. end
        , succeed identity
            |= getChompedString (chompWhile isWhitespace)
            |> andThen
                (\whitespace ->
                    if String.isEmpty whitespace then
                        -- No whitespace means no continuation line - stop without consuming
                        succeed (Done (List.reverse acc))

                    else
                        -- Has whitespace, so it's a continuation line
                        (dbg "header:continuation" whitespace
                            |> (\_ -> succeed identity)
                        )
                            |= getChompedString (chompWhile (\c -> c /= '\n'))
                            |. symbol "\n"
                            |> map (\line -> Loop (String.trimLeft line :: acc))
                )
        , succeed (Done (List.reverse acc))
        ]


{-| Parse the `Date:` header using `Imf.DateTime.parser`.
-}
dateP : Parser Posix
dateP =
    succeed identity
        |. symbol "Date: "
        |= Imf.DateTime.parser
        |. takeWhile (\c -> c /= '\n')
        |. symbol "\n"



-- (\remainder ->
--     case parseTimestamp remainder of
--         Just date ->
--             succeed date
--         Nothing ->
--             problem ("Could not parse date: " ++ remainder)
-- )


{-| Parse the `Subject:` header, including folded lines.
-}
subjectP : Parser String
subjectP =
    succeed identity
        |. symbol "Subject: "
        |= lineRemainderP


{-| Parse the `In-Reply-To:` header and extract only the first token (usually the ID).
-}
inReplyToP : Parser String
inReplyToP =
    succeed
        (\lineRemainder ->
            -- Sometimes this looks like
            --
            --   In-Reply-To: <ID> (So-and-so's message of "Some date")
            --
            -- and we want to extract only the ID.
            case String.split " " lineRemainder of
                first :: _ ->
                    first

                [] ->
                    lineRemainder
        )
        |. symbol "In-Reply-To: "
        |= lineRemainderP


{-| Parse the `References:` header.
-}
referencesP : Parser String
referencesP =
    succeed identity
        |. symbol "References: "
        |= lineRemainderP


{-| Parse the `Message-ID:` header, trimming surrounding '<' '>' if present.
-}
messageIdP : Parser String
messageIdP =
    succeed identity
        |. symbol "Message-ID: "
        |= lineRemainderP
        |> map
            (\id ->
                id
                    |> String.trim
                    |> (\s ->
                            if String.startsWith "<" s && String.endsWith ">" s then
                                String.dropLeft 1 s |> String.dropRight 1

                            else
                                s
                       )
            )



{- Lookahead for a header start. Currently unused. -}
-- lookAheadHeaderP : Parser ()
-- lookAheadHeaderP =
--     -- Look ahead to see if there's a header starting
--     backtrackable (symbol "From")


{-| Consume the "next part" separator block (attachments info).
-}
nextPartP : Parser ()
nextPartP =
    succeed ()
        |. symbol "-------------- next part --------------"
        |. loop () (traceLoop "nextPart" nextPartLoop)


nextPartLoop : () -> Parser (Step () ())
nextPartLoop _ =
    oneOf
        [ end |> map (\_ -> Done ())
        , backtrackable
            (getChompedString (chompWhile (\c -> c /= '\n'))
                |> andThen
                    (\line ->
                        -- Check if this line starts with "From " (preamble, not "From: ")
                        -- This indicates the start of a new message
                        if String.startsWith "From " line && not (String.startsWith "From: " line) then
                            -- Found new message preamble, backtrack and stop
                            dbg "nextPartLoop:new-message" line
                                |> (\_ -> problem "Found new message")

                        else
                            -- Not a "From " preamble line, this is valid content to consume
                            succeed line
                    )
                |> andThen
                    (\_ ->
                        -- Consume the newline or end
                        oneOf
                            [ symbol "\n" |> map (\_ -> dbg "nextPartLoop:loop" () |> (\_ -> Loop ()))
                            , end |> map (\_ -> dbg "nextPartLoop:done" () |> (\_ -> Done ()))
                            ]
                    )
            )
            |> map (\step -> step)
        , succeed (Done ())
        ]


{-| Parse the message body until the next message or until end, respecting
attachment separators and avoiding false preamble matches in the body.
-}
contentP : Parser String
contentP =
    loop [] (traceLoop "content" contentParserLoop)
        |> map String.concat
        |> andThen
            (\content ->
                oneOf
                    [ nextPartP |> map (\_ -> content)
                    , succeed content
                    ]
            )


contentParserLoop : List String -> Parser (Step (List String) (List String))
contentParserLoop acc =
    oneOf
        [ succeed (Done (List.reverse acc))
            |. end
        , backtrackable
            (symbol "-------------- next part --------------"
                |> andThen
                    (\_ ->
                        dbg "content:next-part" { accLen = List.length acc }
                            |> (\_ -> problem "Found next part separator")
                    )
            )
            |> andThen (\_ -> succeed (Done (List.reverse acc)))
        , backtrackable
            (getChompedString (chompWhile (\c -> c /= '\n'))
                |> andThen
                    (\ln ->
                        -- Check if this line starts with "From " (preamble, not "From: ")
                        -- Only cut if it looks like a true mbox preamble; otherwise include in content
                        if isMboxPreamble ln then
                            -- This is the start of a new message, backtrack and stop
                            dbg "content:new-message" (String.left 80 ln)
                                |> (\_ -> problem "Found new message")

                        else
                            -- Not a "From " preamble line, this is valid content
                            succeed ln
                    )
                |> andThen
                    (\ln ->
                        -- Consume the newline or end
                        oneOf
                            [ symbol "\n"
                                |> map
                                    (\_ ->
                                        let
                                            newAcc =
                                                (ln ++ "\n") :: acc
                                        in
                                        dbg "content:loop" { appendedLen = String.length ln, accLen = List.length newAcc }
                                            |> (\_ -> Loop newAcc)
                                    )
                            , end
                                |> map
                                    (\_ ->
                                        let
                                            final =
                                                List.reverse (ln :: acc)
                                        in
                                        dbg "content:done" { lines = List.length final }
                                            |> (\_ -> Done final)
                                    )
                            ]
                    )
            )
            |> andThen
                (\step ->
                    -- If we get here, backtrackable succeeded, so we consumed a line
                    succeed step
                )
        , succeed (Done (List.reverse acc))
        ]


{-| Parse and assemble a full message from header and content.
-}
headerP : Parser Header
headerP =
    succeed Header
        |. preambleP
        |= authorP
        |= dateP
        |= subjectP
        |= oneOf
            [ inReplyToP |> map Just
            , succeed Nothing
            ]
        |= oneOf
            [ referencesP |> map Just
            , succeed Nothing
            ]
        |= messageIdP


{-| Parse a single message and normalize content.
-}
messageP : Parser Message
messageP =
    succeed (\header content -> createMessage header content)
        |= headerP
        |= contentP


{-| Create the final Message record, normalizing quoted and signature lines.
-}
createMessage : Header -> String -> Message
createMessage header content =
    let
        processedContent =
            content
                |> String.trim
                |> String.lines
                |> List.reverse
                |> splitQuotedLines
                |> cleanSignatureLines
                |> List.reverse
                |> String.join "\n"

        message =
            { content = processedContent
            , author = header.author
            , subject = header.subject
            , inReplyTo = header.inReplyTo
            , references = header.references
            , messageId = header.messageId
            , date = header.date
            }
    in
    message


splitQuotedLines : List String -> ( List String, List String )
splitQuotedLines lines =
    splitQuotedLinesHelp lines []


splitQuotedLinesHelp : List String -> List String -> ( List String, List String )
splitQuotedLinesHelp remaining quoted =
    case remaining of
        [] ->
            ( quoted, [] )

        line :: rest ->
            if String.startsWith "> " line || line == "" then
                splitQuotedLinesHelp rest (line :: quoted)

            else
                ( quoted, remaining )


cleanSignatureLines : ( List String, List String ) -> List String
cleanSignatureLines ( quoted, rest ) =
    case ( quoted, rest ) of
        ( [], r ) ->
            r

        ( _, r ) ->
            dropSignaturePrefix r


dropSignaturePrefix : List String -> List String
dropSignaturePrefix lines =
    case lines of
        [] ->
            []

        line :: rest ->
            if String.startsWith "On " line && String.endsWith "> wrote:" line then
                rest

            else
                lines
