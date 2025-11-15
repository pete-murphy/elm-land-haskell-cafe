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


run : String -> Result (List Parser.DeadEnd) Message
run =
    Parser.run messageP



-- Types


type alias Header =
    { author : String
    , date : Posix
    , subject : String
    , messageId : String
    , inReplyTo : Maybe String
    , references : Maybe String
    }


type alias Message =
    { content : String
    , author : String
    , subject : String
    , messageId : String
    , inReplyTo : Maybe String
    , references : Maybe String
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
    getChompedString (chompWhile (\_ -> True))
        |> andThen
            (\chunk ->
                oneOf
                    [ symbol "\n" |> map (\_ -> chunk)
                    , end |> map (\_ -> chunk)
                    ]
            )


lineRemainderP : Parser String
lineRemainderP =
    succeed (::)
        |= singleLineRemainder
        |= loop [] collectLineRemainders
        |> map (String.join " ")


collectLineRemainders : List String -> Parser (Step (List String) (List String))
collectLineRemainders acc =
    oneOf
        [ succeed (\line -> Loop (line :: acc))
            |. chompWhile isWhitespace
            |= singleLineRemainder
        , succeed (Done (List.reverse acc))
        ]


dateP : Parser Posix
dateP =
    succeed identity
        |. symbol "Date: "
        |= Imf.DateTime.parser
        |. lineRemainderP



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


lookAheadHeaderP : Parser ()
lookAheadHeaderP =
    -- Look ahead to see if there's a header starting
    succeed ()
        |. backtrackable (symbol "From")


nextPartP : Parser ()
nextPartP =
    succeed ()
        |. symbol "-------------- next part --------------"
        |. loop () nextPartLoop


nextPartLoop : () -> Parser (Step () ())
nextPartLoop _ =
    oneOf
        [ lookAheadHeaderP |> map (\_ -> Done ())
        , end |> map (\_ -> Done ())
        , getChompedString (chompWhile (\_ -> True)) |> map (\_ -> Loop ())
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
            |. lookAheadHeaderP
        , succeed (Done (List.reverse acc))
            |. end
        , succeed (Done (List.reverse acc))
            |. backtrackable (symbol "-------------- next part --------------")
        , succeed (\chunk -> Loop (chunk :: acc))
            |= (getChompedString (chompWhile (\_ -> True))
                    |> andThen
                        (\line ->
                            oneOf
                                [ symbol "\n" |> map (\_ -> line ++ "\n")
                                , end |> map (\_ -> line)
                                ]
                        )
               )
        ]


headerP : Parser Header
headerP =
    succeed Header
        |. preambleP
        |= authorP
        |= dateP
        |= subjectP
        |= messageIdP
        |= oneOf
            [ inReplyToP |> map Just
            , succeed Nothing
            ]
        |= oneOf
            [ referencesP |> map Just
            , succeed Nothing
            ]


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
    in
    { content = processedContent
    , author = header.author
    , subject = header.subject
    , messageId = header.messageId
    , inReplyTo = header.inReplyTo
    , references = header.references
    , date = header.date
    }


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
