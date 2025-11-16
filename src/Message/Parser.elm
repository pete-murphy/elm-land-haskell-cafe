module Message.Parser exposing
    ( Header
    , Message
    , headerP
    , messageP
    , run
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


run : String -> Result (List Parser.DeadEnd) (List Message)
run =
    Parser.run messagesP


messagesP : Parser (List Message)
messagesP =
    loop [] collectMessages


collectMessages : List Message -> Parser (Step (List Message) (List Message))
collectMessages acc =
    oneOf
        [ succeed identity
            |= messageP
            |> andThen
                (\msg ->
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


type alias Header =
    { author : String
    , date : Posix
    , subject : String
    , inReplyTo : Maybe String
    , references : Maybe String
    , messageId : String
    }


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


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t'


takeWhile : (Char -> Bool) -> Parser String
takeWhile predicate =
    getChompedString (chompWhile predicate)



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


preambleP : Parser ()
preambleP =
    succeed ()
        |. symbol "From"
        |. takeWhile (\c -> c /= '\n')
        |. symbol "\n"


authorP : Parser String
authorP =
    succeed identity
        |. symbol "From: "
        |. takeWhile (\c -> c /= '(')
        |= betweenParens
        |. symbol "\n"


betweenParens : Parser String
betweenParens =
    succeed identity
        |. symbol "("
        |= takeWhile (\c -> c /= ')')
        |. symbol ")"


singleLineRemainder : Parser String
singleLineRemainder =
    oneOf
        [ succeed identity
            |= getChompedString (chompWhile (\c -> c /= '\n'))
            |. symbol "\n"
        , succeed identity
            |= getChompedString (chompWhile (\_ -> True))
            |. end
        ]


lineRemainderP : Parser String
lineRemainderP =
    succeed (::)
        |= singleLineRemainder
        |= loop [] collectLineRemainders
        |> map (String.join " ")


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
                        succeed identity
                            |= getChompedString (chompWhile (\c -> c /= '\n'))
                            |. symbol "\n"
                            |> map (\line -> Loop (line :: acc))
                )
        , succeed (Done (List.reverse acc))
        ]


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


subjectP : Parser String
subjectP =
    succeed identity
        |. symbol "Subject: "
        |= lineRemainderP


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


referencesP : Parser String
referencesP =
    succeed identity
        |. symbol "References: "
        |= lineRemainderP


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


lookAheadHeaderP : Parser ()
lookAheadHeaderP =
    -- Look ahead to see if there's a header starting
    backtrackable (symbol "From")


nextPartP : Parser ()
nextPartP =
    succeed ()
        |. symbol "-------------- next part --------------"
        |. loop () nextPartLoop


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
                            problem "Found new message"

                        else
                            -- Not a "From " preamble line, this is valid content to consume
                            succeed line
                    )
                |> andThen
                    (\line ->
                        -- Consume the newline or end
                        oneOf
                            [ symbol "\n" |> map (\_ -> Loop ())
                            , end |> map (\_ -> Done ())
                            ]
                    )
            )
            |> map (\step -> step)
        , succeed (Done ())
        ]


contentP : Parser String
contentP =
    loop [] contentParserLoop
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
                |> andThen (\_ -> problem "Found next part separator")
            )
            |> andThen (\_ -> succeed (Done (List.reverse acc)))
        , backtrackable
            (getChompedString (chompWhile (\c -> c /= '\n'))
                |> andThen
                    (\line ->
                        -- Check if this line starts with "From " (preamble, not "From: ")
                        -- This indicates the start of a new message
                        if String.startsWith "From " line && not (String.startsWith "From: " line) then
                            -- This is the start of a new message, backtrack and stop
                            problem "Found new message"

                        else
                            -- Not a "From " preamble line, this is valid content
                            succeed line
                    )
                |> andThen
                    (\line ->
                        -- Consume the newline or end
                        oneOf
                            [ symbol "\n"
                                |> map (\_ -> Loop ((line ++ "\n") :: acc))
                            , end
                                |> map (\_ -> Done (List.reverse (line :: acc)))
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


messageP : Parser Message
messageP =
    succeed (\header content -> createMessage header content)
        |= headerP
        |= contentP


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
