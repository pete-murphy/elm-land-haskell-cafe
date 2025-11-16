module ParserTest exposing (..)

import Expect
import Message.Parser
import Test exposing (Test)


test : Test
test =
    Test.describe "Parser"
        [ Test.test "parse sample" <|
            \_ ->
                Message.Parser.run sample
                    |> Expect.all
                        [ Expect.ok
                        , Result.map List.length >> Expect.equal (Ok 14)
                        ]
        , Test.test "parse all Message-IDs from sample" <|
            \_ ->
                let
                    expectedMessageIds =
                        [ "CAAmpXihPo=+pyBtQQqjNjGYYwR1yyiPjFaAuGNHui48E_K-2hg@mail.gmail.com"
                        , "aI-1wAcOCYOFthbp@tauhou"
                        , "CAJoPsuA0yVW1dnv+ue+qjogMBE65pmeYbB_FQ2KWtS0-xLkM9g@mail.gmail.com"
                        , "C41ABEC2-35C6-4708-8445-26F1C00F1C1D@nothos.net"
                        , "2pvhnbxa5lneok2e3ohskt3tcntqpoclxmptdk2qoquack5ovy@ofgaqxjmynll"
                        , "CABjRk3mvPUyn2onK9bsmNmtgrrAOi=6nAwCizw9MD_JAN1vFBg@mail.gmail.com"
                        , "CAEDhREoOthJ9W3N63qRc9vH_=xHmj8EbyfT-tBCZdkxQQfSpXA@mail.gmail.com"
                        , "CAEDhRErB8gxrwt4gMiuaE0H8HA71KB9jNtpNKjktPx6wuwoS5w@mail.gmail.com"
                        , "C08D6373-4F20-49FD-BED9-9FFE2BC99B40@mac.com"
                        , "2F70BC4F-AB96-45F8-B644-A7B47749EDD1@mac.com"
                        , "jc7mraxljldxz2xp7b6f57yfhimdxladjwjoyzxrykkqqejjsv@g2wojh42lusc"
                        , "aJXxKmpCL519syLf@chardros.imrryr.org"
                        , "ufsiejujiege6jwm5cxdzqiqsdzox2cwlzzbylc4ibrosnzgbq@ofh6vr5idhlk"
                        , "DA73F8B8-1519-4657-9128-85D802541884@gmail.com"
                        ]
                in
                Message.Parser.run sample
                    |> Result.map
                        (\messages ->
                            Expect.all
                                ((\_ -> List.length messages |> Expect.equal (List.length expectedMessageIds))
                                    :: List.map2
                                        (\expectedId message ->
                                            \_ ->
                                                message.messageId
                                                    |> Expect.equal expectedId
                                        )
                                        expectedMessageIds
                                        messages
                                )
                        )
                    |> Result.map (\expectation -> expectation ())
                    |> Result.withDefault (Expect.fail "Parser returned error")
        , Test.test "all messages have required fields" <|
            \_ ->
                Message.Parser.run sample
                    |> Result.map
                        (\messages ->
                            messages
                                |> List.indexedMap
                                    (\i message ->
                                        Expect.all
                                            [ \_ -> String.isEmpty message.messageId |> Expect.equal False |> Expect.onFail ("Message " ++ String.fromInt (i + 1) ++ " missing messageId")
                                            , \_ -> String.isEmpty message.author |> Expect.equal False |> Expect.onFail ("Message " ++ String.fromInt (i + 1) ++ " missing author")
                                            , \_ -> String.isEmpty message.subject |> Expect.equal False |> Expect.onFail ("Message " ++ String.fromInt (i + 1) ++ " missing subject")
                                            ]
                                    )
                                |> List.map (\expectation -> expectation ())
                                |> List.foldl (\e acc -> Expect.all [ \_ -> e, \_ -> acc () ]) (\_ -> Expect.pass)
                        )
                    |> Result.map (\expectation -> expectation ())
                    |> Result.withDefault (Expect.fail "Parser returned error")
        , Test.describe "edge cases" <|
            [ Test.test "body line starting with 'From ' is not a new message" <|
                \_ ->
                    let
                        input =
                            String.join "\n"
                                [ "From user@example.com Sun Aug  3 14:14:10 2025"
                                , "From: user@example.com (Example User)"
                                , "Date: Sun, 3 Aug 2025 10:14:10 -0400"
                                , "Subject: From in body"
                                , "Message-ID: <id-1@example.com>"
                                , ""
                                , "Regular line"
                                , "From the team"
                                , "End."
                                ]
                    in
                    case Message.Parser.run input of
                        Ok msgs ->
                            case msgs of
                                [ one ] ->
                                    Expect.all
                                        [ \_ -> Expect.equal "From in body" one.subject
                                        , \_ -> Expect.equal True (String.contains "From the team" one.content)
                                        ]
                                        |> (\f -> f ())

                                _ ->
                                    Expect.fail "Expected exactly one message"

                        Err _ ->
                            Expect.fail "Should parse"
            , Test.test "nested parentheses in From display name" <|
                \_ ->
                    let
                        input =
                            String.join "\n"
                                [ "From user@example.com Sun Aug  3 14:14:10 2025"
                                , "From: user@example.com (User (Team) Name)"
                                , "Date: Sun, 3 Aug 2025 10:14:10 -0400"
                                , "Subject: Nested parens"
                                , "Message-ID: <id-2@example.com>"
                                , ""
                                , "Hello"
                                ]
                    in
                    Message.Parser.run input
                        |> Result.map (\msgs -> List.length msgs)
                        |> Expect.equal (Ok 1)
            , Test.test "folded Subject header with continuation" <|
                \_ ->
                    let
                        input =
                            String.join "\n"
                                [ "From user@example.com Sun Aug  3 14:14:10 2025"
                                , "From: user@example.com (Example User)"
                                , "Date: Sun, 3 Aug 2025 10:14:10 -0400"
                                , "Subject: Line one"
                                , " continuation"
                                , "Message-ID: <id-3@example.com>"
                                , ""
                                , "Hi"
                                ]
                    in
                    case Message.Parser.run input of
                        Ok [ one ] ->
                            Expect.equal "Line one continuation" one.subject

                        Ok _ ->
                            Expect.fail "Expected one message"

                        Err _ ->
                            Expect.fail "Should parse"
            , Test.test "unicode in Subject header" <|
                \_ ->
                    let
                        input =
                            String.join "\n"
                                [ "From user@example.com Sun Aug  3 14:14:10 2025"
                                , "From: user@example.com (例 ユーザー)"
                                , "Date: Sun, 3 Aug 2025 10:14:10 -0400"
                                , "Subject: Hello 🌟世界"
                                , "Message-ID: <id-4@example.com>"
                                , ""
                                , "Hi"
                                ]
                    in
                    Message.Parser.run input
                        |> Result.map (\msgs -> List.length msgs)
                        |> Expect.equal (Ok 1)
            ]
        ]


sample =
    """From icfp.publicity at googlemail.com  Sun Aug  3 14:14:10 2025
From: icfp.publicity at googlemail.com (ICFP Publicity)
Date: Sun, 3 Aug 2025 10:14:10 -0400
Subject: [Haskell-cafe] ICFP 2025 (Co-Located with SPLASH!): Call for
\tParticipation
Message-ID: <CAAmpXihPo=+pyBtQQqjNjGYYwR1yyiPjFaAuGNHui48E_K-2hg@mail.gmail.com>

==================================================
Call for Participation

The 30th ACM SIGPLAN International Conference
on Functional Programming (ICFP 2025) and
affiliated events

*** This year, co-located with SPLASH for a ***
*** joint ICFP/SPLASH Conference ***

https://icfp25.sigplan.org
Singapore; Oct 12-18, 2025

*** Register by Aug 31, 2025 for a discount! ***
==================================================

ICFP is a celebration of the art and science of
functional programming, providing a forum for
researchers and developers to engage on a variety
of topics, from foundations to features, and from
abstraction to application.

This year, for the first time in history, ICFP will
be co-located with SPLASH and its satellite events
as a part of the joint ICFP/SPLASH 2025 conference.

You are invited to participate in a full week of
events dedicated to the art and science of programming,
featuring both the ICFP and SPLASH main conferences
as well as numerous related events.

* ICFP Accepted Papers:
https://icfp25.sigplan.org/track/icfp-2025-papers#event-overview

* Registration:
https://icfp25.sigplan.org/attending/registration
*The early-bird deadline is August 31, 2025*
(Additionally, there is a full week discount
if you register for 7 days!)

* Venue (Sunday Workshops only): NUS School of Computing
https://icfp25.sigplan.org/venue/splash-2025-venue1

* Venue (Main Conference): Marina Bay Sands Convention Centre
https://icfp25.sigplan.org/venue/splash-2025-venue


Full list of events:

October 12
Tutorials
-
https://conf.researchr.org/track/icfp-splash-2025/icfp-splash-2025-tutorials
PLMW
- https://conf.researchr.org/home/icfp-splash-2025/plmw-icfp-splash-2025
TyDE
- https://conf.researchr.org/home/icfp-splash-2025/tyde-2025
FARM
- https://2025.splashcon.org/track/splash-2025-farm
HOPE
- https://conf.researchr.org/home/icfp-splash-2025/hope-2025
Erlang
- https://conf.researchr.org/home/icfp-splash-2025/erlang-2025
FUNARCH
- https://conf.researchr.org/home/icfp-splash-2025/funarch-2025

October 13-15
ICFP (3 days)
- https://icfp25.sigplan.org/

October 13-14
SAS (2 days)
- https://2025.splashcon.org/home/sas-2025

October 13
SPLASH Doctoral Symposium
- https://2025.splashcon.org/track/splash-2025-Doctoral-Symposium
SCALA
- https://conf.researchr.org/home/icfp-splash-2025/scala-2025
PROPL
- https://conf.researchr.org/home/icfp-splash-2025/propl-2025

October 14-15
OlivierFest (2 days)
- https://conf.researchr.org/home/icfp-splash-2025/olivierfest-2025
October 14
HATRA
- https://conf.researchr.org/home/icfp-splash-2025/hatra-2025
MPLR
- https://conf.researchr.org/home/icfp-splash-2025/mplr-2025
IWACO
- https://conf.researchr.org/home/icfp-splash-2025/iwaco-2025
October 15
PAINT
- https://conf.researchr.org/home/icfp-splash-2025/paint-2025
LMPL
- https://conf.researchr.org/home/icfp-splash-2025/lmpl-2025
VMIL
- https://conf.researchr.org/home/icfp-splash-2025/vmil-2025

October 16-18
OOPSLA (3 days)
- https://2025.splashcon.org/track/OOPSLA
ONWARD! (3 days)
- https://2025.splashcon.org/track/splash-2025-Onward-papers

October 16-17
HASKELL (2 days)
- https://conf.researchr.org/home/icfp-splash-2025/haskellsymp-2025

October 16
ML Family
- https://conf.researchr.org/home/icfp-splash-2025/mlsymposium-2025
SCHEME
- https://conf.researchr.org/home/icfp-splash-2025/scheme-2025
WASM
- https://conf.researchr.org/home/icfp-splash-2025/webassembly-ws-2025

October 17
OCAML
- https://conf.researchr.org/home/icfp-splash-2025/ocaml-2025
MINI KANREN
- https://conf.researchr.org/home/icfp-splash-2025/minikanren-2025
October 18
SPLASH-E
- https://2025.splashcon.org/track/splash-2025-SPLASH-E
REBASE
- https://conf.researchr.org/home/icfp-splash-2025/rebase-2025
Industry Forum
- https://conf.researchr.org/home/icfp-splash-2025/industry-forum-2025


ICFP 2025 will feature three keynotes:

Ekaterina Komendantskaya:
Proof-Carrying Neuro-Symbolic Code
https://icfp25.sigplan.org/details/icfp-2025-icfp-keynotes/2/Proof-Carrying-Neuro-Symbolic-Code

Christos Dimoulas:
The Rational Programmer, A Method for Investigating Programming Language
Pragmatics
https://icfp25.sigplan.org/details/icfp-2025-icfp-keynotes/3/The-Rational-Programmer-A-Method-for-Investigating-Programming-Language-Pragmatics

Satnam Singh:
Functional Programming for Hardware Design
https://icfp25.sigplan.org/details/icfp-2025-icfp-keynotes/1/Functional-Programming-for-Hardware-Design


ICFP 2025 conference organizers:

https://icfp25.sigplan.org/committee/icfp-2025-organizing-committee


We hope to see you in Singapore in October. Don't forget
to register by *August 31, 2025* for the early-bird
discount! Additionally, there is a *full-week discount*:
register for the whole 7 days and only pay for 6!

https://icfp25.sigplan.org/attending/registration
-------------- next part --------------
An HTML attachment was scrubbed...
URL: <http://mail.haskell.org/pipermail/haskell-cafe/attachments/20250803/8f21aed1/attachment.html>

From haskell at stefan-klinger.de  Sun Aug  3 19:17:20 2025
From: haskell at stefan-klinger.de (Stefan Klinger)
Date: Sun, 3 Aug 2025 21:17:20 +0200
Subject: [Haskell-cafe] Correct parsers for bounded integral values
In-Reply-To: <aHOZt6DINagivTzE@tauhou>
References: <aHOZt6DINagivTzE@tauhou>
Message-ID: <aI-1wAcOCYOFthbp@tauhou>

Pierre Thierry (2025-Jul-21, excerpt):
> Well, IIUC, it conforms to the Haskell 98 specification.

Hm.  Maybe I keep missing something here.  After all it's beeing
pointed out repeatedly by you guys (and yes, I do listen), but I fail
to verify that claim.

I don't think the Haskell Spec really *requires* the `read` function
for a bounded integral type to wrap around.

Jeff Clites via Haskell-Cafe (2025-Jul-21, excerpt):
> I think you will find, though, that `read @Word8` is intended to
> match the behavior of `(fromInteger @Word8) . (read @Integer)`. At
> least, the `Int` case is specified in the Haskell Report (in the
> Prelude implementation).

Sorry, I cannot find this.  Would you have a URL and a line number for
me?  I'm really sorry my google-foo seems to be insufficient.

I have found in the 98 report [4], similar in 2010 [5]:

    The results of exceptional conditions (such as overflow or
    underflow) on the fixed-precision numeric types are undefined; an
    implementation may choose error (_|_, semantically), a truncated
    value, or a special value such as infinity, indefinite, etc.

So, not violating the report, but the implementation might choose to
do better.

Cheers
Stefan


[1]: https://mail.haskell.org/pipermail/haskell-cafe/2025-July/137156.html
[2]: https://mail.haskell.org/pipermail/haskell-cafe/2025-July/137155.html
[3]: https://mail.haskell.org/pipermail/haskell-cafe/2025-July/137162.html
[4]: https://www.haskell.org/onlinereport/basic.html#sect6.4
[5]: https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1350006.4


--
Stefan Klinger, Ph.D. -- computer scientist              o/X
http://stefan-klinger.de                                 /\\/
https://github.com/s5k6                                    \\
I prefer receiving plain text messages, not exceeding 32kB.

From tanuki at gmail.com  Sun Aug  3 23:46:12 2025
From: tanuki at gmail.com (Akhra Gannon)
Date: Sun, 3 Aug 2025 16:46:12 -0700
Subject: [Haskell-cafe] Correct parsers for bounded integral values
In-Reply-To: <aI-1wAcOCYOFthbp@tauhou>
References: <aHOZt6DINagivTzE@tauhou> <aI-1wAcOCYOFthbp@tauhou>
Message-ID: <CAJoPsuA0yVW1dnv+ue+qjogMBE65pmeYbB_FQ2KWtS0-xLkM9g@mail.gmail.com>

I believe it's not that integers must wrap, but that Read behavior must
match compiler behavior (and GHC's behavior is to wrap).

On Sun, Aug 3, 2025, 12:17â€¯PM Stefan Klinger <haskell at stefan-klinger.de>
wrote:

> Pierre Thierry (2025-Jul-21, excerpt):
> > Well, IIUC, it conforms to the Haskell 98 specification.
>
> Hm.  Maybe I keep missing something here.  After all it's beeing
> pointed out repeatedly by you guys (and yes, I do listen), but I fail
> to verify that claim.
>
> I don't think the Haskell Spec really *requires* the `read` function
> for a bounded integral type to wrap around.
>
> Jeff Clites via Haskell-Cafe (2025-Jul-21, excerpt):
> > I think you will find, though, that `read @Word8` is intended to
> > match the behavior of `(fromInteger @Word8) . (read @Integer)`. At
> > least, the `Int` case is specified in the Haskell Report (in the
> > Prelude implementation).
>
> Sorry, I cannot find this.  Would you have a URL and a line number for
> me?  I'm really sorry my google-foo seems to be insufficient.
>
> I have found in the 98 report [4], similar in 2010 [5]:
>
>     The results of exceptional conditions (such as overflow or
>     underflow) on the fixed-precision numeric types are undefined; an
>     implementation may choose error (_|_, semantically), a truncated
>     value, or a special value such as infinity, indefinite, etc.
>
> So, not violating the report, but the implementation might choose to
> do better.
>
> Cheers
> Stefan
>
>
> [1]: https://mail.haskell.org/pipermail/haskell-cafe/2025-July/137156.html
> [2]: https://mail.haskell.org/pipermail/haskell-cafe/2025-July/137155.html
> [3]: https://mail.haskell.org/pipermail/haskell-cafe/2025-July/137162.html
> [4]: https://www.haskell.org/onlinereport/basic.html#sect6.4
> [5]:
> https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1350006.4
>
>
> --
> Stefan Klinger, Ph.D. -- computer scientist              o/X
> http://stefan-klinger.de                                 /\\/
> https://github.com/s5k6                                    \\
> I prefer receiving plain text messages, not exceeding 32kB.
> _______________________________________________
> Haskell-Cafe mailing list
> To (un)subscribe, modify options or view archives go to:
> http://mail.haskell.org/cgi-bin/mailman/listinfo/haskell-cafe
> Only members subscribed via the mailman list are allowed to post.
-------------- next part --------------
An HTML attachment was scrubbed...
URL: <http://mail.haskell.org/pipermail/haskell-cafe/attachments/20250803/3a7151d2/attachment.html>

From pierre at nothos.net  Mon Aug  4 01:23:54 2025
From: pierre at nothos.net (Pierre Thierry)
Date: Mon, 04 Aug 2025 03:23:54 +0200
Subject: [Haskell-cafe] Correct parsers for bounded integral values
In-Reply-To: <aI-1wAcOCYOFthbp@tauhou>
References: <aHOZt6DINagivTzE@tauhou> <aI-1wAcOCYOFthbp@tauhou>
Message-ID: <C41ABEC2-35C6-4708-8445-26F1C00F1C1D@nothos.net>

On August 3, 2025 9:17:20 PM GMT+02:00, Stefan Klinger <haskell at stefan-klinger.de> wrote:
>> Well, IIUC, it conforms to the Haskell 98 specification.
>I don't think the Haskell Spec really *requires* the `read` function
>for a bounded integral type to wrap around.

The Haskell 98 report says that reading numerical literals is defined as applying `fromInteger` in section 6.4.1 and section 6.4 says an implementation may choose to use a truncated value on overflow (or may choose anything else, it's undefined). Truncating an unbounded integer to a fixed sized would produce wrapping around.

<https://www.haskell.org/onlinereport/basic.html#sect6.4.1>

Curiously,
Pierre Thierry 
-------------- next part --------------
An HTML attachment was scrubbed...
URL: <http://mail.haskell.org/pipermail/haskell-cafe/attachments/20250804/7349ecf6/attachment.html>

From zubin at well-typed.com  Wed Aug  6 10:20:57 2025
From: zubin at well-typed.com (Zubin Duggal)
Date: Wed, 6 Aug 2025 15:50:57 +0530
Subject: [Haskell-cafe] GHC 9.10.3-rc2 is now available
Message-ID: <2pvhnbxa5lneok2e3ohskt3tcntqpoclxmptdk2qoquack5ovy@ofgaqxjmynll>

The GHC developers are very pleased to announce the availability
of the second release candidate for GHC 9.10.3. Binary distributions, source
distributions, and documentation are available at [downloads.haskell.org][] and
via [GHCup](https://www.haskell.org/ghcup/).

GHC 9.10.3 is a bug-fix release fixing over 50 issues of a variety of
severities and scopes. A full accounting of these fixes can be found in the
[release notes][]. As always, GHC's release status, including planned future
releases, can be found on the GHC Wiki [status][].

The changes from the first release candidate are:

- Bumping the text submodule to 2.1.3
- Reverting a bug fix (!14291) that restricted previously allowed namespace specifiers (#26250)
- Reverting the bump of the deepseq submodule to 1.5.2.0 (#26251)

This release candidate will have a two-week testing period. If all goes well
the final release will be available the week of 19 August 2025.

We would like to thank Well-Typed, Tweag I/O, Juspay, QBayLogic, Channable,
Serokell, SimSpace, the Haskell Foundation, and other anonymous contributors
whose on-going financial and in-kind support has facilitated GHC maintenance
and release management over the years. Finally, this release would not have
been possible without the hundreds of open-source contributors whose work
comprise this release.

As always, do give this release a try and open a [ticket][] if you see
anything amiss.


[release notes]: https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.10/docs/users_guide/9.10.3-notes.rst?ref_type=heads&plain=1
[status]: https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC-status
[downloads.haskell.org]: https://downloads.haskell.org/ghc/9.10.3-rc2
[ticket]: https://gitlab.haskell.org/ghc/ghc/-/issues/new
-------------- next part --------------
A non-text attachment was scrubbed...
Name: signature.asc
Type: application/pgp-signature
Size: 488 bytes
Desc: not available
URL: <http://mail.haskell.org/pipermail/haskell-cafe/attachments/20250806/daa5b4b1/attachment.sig>

From xnningxie at gmail.com  Wed Aug  6 18:12:45 2025
From: xnningxie at gmail.com (Ningning Xie)
Date: Wed, 6 Aug 2025 14:12:45 -0400
Subject: [Haskell-cafe] POPL 2026 Call For Tutorials
Message-ID: <CABjRk3mvPUyn2onK9bsmNmtgrrAOi=6nAwCizw9MD_JAN1vFBg@mail.gmail.com>

POPL 2026 CALL FOR TUTORIALS

https://popl26.sigplan.org/track/POPL-2026-tutorials#Call-For-Tutorials

The 53rd ACM SIGPLAN Symposium on Principles of Programming Languages (POPL
2026) will be held in Rennes, France.

POPL provides a forum for the discussion of fundamental principles and
important innovations in the design, definition, analysis, transformation,
implementation, and verification of programming languages, programming
systems, and programming abstractions.

Tutorials for POPL 2026 are solicited on any topic relevant to the POPL
community. We particularly encourage submissions of introductory tutorials
that make the research presented at POPL more accessible to the
participants.

Tutorials will be held on Jan 11â€“13, 2026. The expected length of a
tutorial is 3 hours, including questions and discussion (Q&A).

Submission details

Deadline for submission: October 10th, 2025

Notification of acceptance: October 24th, 2025

A tutorial proposal should provide the following information:

   - Tutorial title
   - Presenter(s), affiliation(s), and contact information
   - 1-3 page description (for evaluation). This should include the
   objectives, topics to be covered, presentation approach, target audience,
   prerequisite knowledge, and if the tutorial was previously held, the
   location (i.e. which conference), date, and number of attendees if
   available.
   - 1-2 paragraph abstract suitable for tutorial publicity.
   - 1-paragraph biography suitable for tutorial publicity.

Proposals must be submitted by email to Robert Rand (rand at uchicago.edu) and
Alan Schmitt (alan.schmitt at inria.fr) with the subject line "POPL 2026
Tutorial Proposal: [tutorial name]". The proposal should be attached as a
PDF, docx, or txt file.

Further information

Any questions regarding POPL 2026 tutorials should be addressed to the
workshops chairs, Robert Rand (rand at uchicago.edu) and Alan Schmitt
(alan.schmitt at inria.fr).
-------------- next part --------------
An HTML attachment was scrubbed...
URL: <http://mail.haskell.org/pipermail/haskell-cafe/attachments/20250806/31d71fbc/attachment.html>

From pikolasikolatest2 at gmail.com  Thu Aug  7 21:48:36 2025
From: pikolasikolatest2 at gmail.com (walid falcon)
Date: Thu, 7 Aug 2025 22:48:36 +0100
Subject: [Haskell-cafe] =?utf-8?b?2LTZh9in2K/YqSDYp9mE2KPYqNmI2Kkg2YHZiiA=?=
\t=?utf-8?b?2KfZhNmF2LrYsdio?=
Message-ID: <CAEDhREoOthJ9W3N63qRc9vH_=xHmj8EbyfT-tBCZdkxQQfSpXA@mail.gmail.com>

Spam detection software, running on the system "mail.haskell.org", has
identified this incoming email as possible spam.  The original message
has been attached to this so you can view it (if it isn't spam) or label
similar future email.  If you have any questions, see
@@CONTACT_ADDRESS@@ for details.

Content preview:  Ã™ÂƒÃ™Â„ Ã™
Ã˜Â§ Ã˜ÂªÃ˜Â\u{00AD}Ã˜ÂªÃ˜Â§Ã˜Â¬ Ã™
Ã˜Â¹Ã˜Â±Ã™Â\u{0081}Ã˜ÂªÃ™Â‡ Ã˜Â\u{00AD}Ã™ÂˆÃ™Â„ Ã˜Â´Ã™Â‡Ã˜Â§Ã˜Â¯Ã˜Â© Ã˜Â§Ã™Â„Ã˜Â£Ã˜Â¨Ã™ÂˆÃ˜Â©
   Ã˜Â¨Ã˜Â§Ã™Â„Ã™
Ã˜ÂºÃ˜Â±Ã˜Â¨ Ã™ÂŠÃ™
Ã™ÂƒÃ™Â†Ã™Âƒ Ã˜ÂªÃ˜Â\u{00AD}Ã™
Ã™ÂŠÃ™Â„ Ã˜Â§Ã™Â„Ã˜Â·Ã™Â„Ã˜Â¨ Ã™
Ã™Â† Ã˜Â®Ã™Â„Ã˜Â§Ã™Â„ Ã˜Â§Ã™Â„Ã˜Â±Ã˜Â§Ã˜Â¨Ã˜Â·
   Ã˜Â§Ã™Â„Ã™
Ã˜Â¨Ã˜Â§Ã˜Â´Ã˜Â± https://www.targir.com/2025/04/blog-post_14.html [...] 

Content analysis details:   (6.6 points, 5.0 required)

 pts rule name              description
---- ---------------------- --------------------------------------------------
 0.0 FREEMAIL_FROM          Sender email is commonly abused enduser mail provider
                            (pikolasikolatest2[at]gmail.com)
-0.0 SPF_PASS               SPF: sender matches SPF record
 0.1 FREEMAIL_ENVFROM_END_DIGIT Envelope-from freemail username ends in
                            digit (pikolasikolatest2[at]gmail.com)
 5.0 UNWANTED_LANGUAGE_BODY BODY: Message written in an undesired language
 0.0 HTML_MESSAGE           BODY: HTML included in message
 1.5 BODY_8BITS             BODY: Body includes 8 consecutive 8-bit characters
 0.0 T_DKIM_INVALID         DKIM-Signature header exists but is not valid
 0.0 T_TO_NO_BRKTS_FREEMAIL T_TO_NO_BRKTS_FREEMAIL

The original message was not completely plain text, and may be unsafe to
open with some email clients; in particular, it may contain a virus,
or confirm that your address can receive spam.  If you wish to view
it, it may be safer to save it to a file and open it with an editor.

-------------- next part --------------
An embedded message was scrubbed...
From: walid falcon <pikolasikolatest2 at gmail.com>
Subject: ????? ?????? ?? ??????
Date: Thu, 7 Aug 2025 22:48:36 +0100
Size: 24478
URL: <http://mail.haskell.org/pipermail/haskell-cafe/attachments/20250807/2d047f91/attachment.mht>

From pikolasikolatest2 at gmail.com  Thu Aug  7 21:49:41 2025
From: pikolasikolatest2 at gmail.com (walid falcon)
Date: Thu, 7 Aug 2025 22:49:41 +0100
Subject: [Haskell-cafe] TargirDotCom
Message-ID: <CAEDhRErB8gxrwt4gMiuaE0H8HA71KB9jNtpNKjktPx6wuwoS5w@mail.gmail.com>

https://www.targir.com/
https://www.targir.com/2025/04/blog-post_14.html
-------------- next part --------------
An HTML attachment was scrubbed...
URL: <http://mail.haskell.org/pipermail/haskell-cafe/attachments/20250807/cdbb81e2/attachment.html>

From jclites at mac.com  Fri Aug  8 00:28:43 2025
From: jclites at mac.com (Jeff Clites)
Date: Thu, 7 Aug 2025 17:28:43 -0700
Subject: [Haskell-cafe] Correct parsers for bounded integral values
In-Reply-To: <aI-1wAcOCYOFthbp@tauhou>
References: <aHOZt6DINagivTzE@tauhou> <aI-1wAcOCYOFthbp@tauhou>
Message-ID: <C08D6373-4F20-49FD-BED9-9FFE2BC99B40@mac.com>

> On Aug 3, 2025, at 12:17 PM, Stefan Klinger <haskell at stefan-klinger.de> wrote:
> 
> Jeff Clites via Haskell-Cafe (2025-Jul-21, excerpt):
>> I think you will find, though, that `read @Word8` is intended to
>> match the behavior of `(fromInteger @Word8) . (read @Integer)`. At
>> least, the `Int` case is specified in the Haskell Report (in the
>> Prelude implementation).
> 
> Sorry, I cannot find this.  Would you have a URL and a line number for
> me?  I'm really sorry my google-foo seems to be insufficient.

This part is what I was thinking of:

    https://www.haskell.org/onlinereport/haskell2010/haskellch9.html#verbatim-307

That's the `Read` instance for `Int`, which defines `readsPrec` (which is used by `reads`, which is used by `read`, also defined there). It uses `fromInteger`.

This is the section of the spec with the Standard Prelude. (It says it's serving as a specification, though not requiring the actual implementation to be what's given here.)

That's only specifically about `Int`. The limited-width types are in these two sections:

    https://www.haskell.org/onlinereport/haskell2010/haskellch18.html
    https://www.haskell.org/onlinereport/haskell2010/haskellch23.html

This is the part of the spec about the standard libraries, though it doesn't show the full implementations. I don't know if they are supplied elsewhere (I've looked and can't find them), though I don't think that that means that their behavior is unspecified. It does mention, "All arithmetic is performed modulo 2^n, where n is the number of bits in the type." So it doesn't supply the `Read` implementations for these other types and it doesn't directly say that `Read` has to wrap even though arithmetic operations do. But I would expect that the intention is that they act consistently with `Int`.

When I said, "I think you will find..." in my original message, what I really meant was that if you file a bug report I think the reply will be that the behavior is as intended (specifically, to match `Int`). But who knows.

Enjoy!

Jeff

From jclites at mac.com  Fri Aug  8 00:37:11 2025
From: jclites at mac.com (Jeff Clites)
Date: Thu, 7 Aug 2025 17:37:11 -0700
Subject: [Haskell-cafe] Correct parsers for bounded integral values
In-Reply-To: <CAKFCL4Xia8X7br6v6WEB91iS-UwKZ2KF6crrhPL0EctQy4bsng@mail.gmail.com>
References: <aHOZt6DINagivTzE@tauhou>
 <bfc6b749-3ced-4e59-a0e1-6d9b24d74177@gmail.com>
 <aH0X_FTnASkCuESt@chardros.imrryr.org>
 <CAKFCL4VAFYRfnh1Ys9N1_eVatN+QSOn+=ggLbYzvkB11GUTTmA@mail.gmail.com>
 <950DD105-A43F-40BA-A93A-D4A649EB52B0@mac.com>
 <CAKFCL4Xia8X7br6v6WEB91iS-UwKZ2KF6crrhPL0EctQy4bsng@mail.gmail.com>
Message-ID: <2F70BC4F-AB96-45F8-B644-A7B47749EDD1@mac.com>

Thanks Brandon for the info. I don't fully understand the implications of having the carry bit set on the resulting 128-bit value, but that's okay.

One thing I noticed about this:

> On Jul 20, 2025, at 5:50 PM, Brandon Allbery <allbery.b at gmail.com> wrote:
> 
> [...]
> That it otherwise behaves as modular is in support of this, not in support of a mathematical law, and the behavior might differ on other architectures (e.g. PPC, or if someone somewhere still has a processor using 1s- complement). Which is the most important point: the Report specifies `Integral` types other than `Integer` to behave like the machine type, and you're basically assuming x86-64 (_probably_ also AArch64 but i haven't studied that one, just assuming it's 2s-complement) if you assume either modular arithmetic or any other behavior. The only promise you get is "matches the underlying hardware".

Actually, this section of the Report:

    https://www.haskell.org/onlinereport/haskell2010/haskellch18.html#x26-22300018

says this:

    This module provides signed integer types of unspecified width (Int) and fixed widths (Int8, Int16, Int32 and Int64). All arithmetic is performed modulo 2^n, where n is the number of bits in the type.


So that seems to contract the idea that they are just meant to match the underlying hardware.

Jeff

From zubin at well-typed.com  Fri Aug  8 12:32:26 2025
From: zubin at well-typed.com (Zubin Duggal)
Date: Fri, 8 Aug 2025 18:02:26 +0530
Subject: [Haskell-cafe] GHC 9.10.3-rc3 is now available
Message-ID: <jc7mraxljldxz2xp7b6f57yfhimdxladjwjoyzxrykkqqejjsv@g2wojh42lusc>

The GHC developers are very pleased to announce the availability
of the third release candidate for GHC 9.10.3. Binary distributions, source
distributions, and documentation are available at [downloads.haskell.org][] and
via [GHCup](https://www.haskell.org/ghcup/).

GHC 9.10.3 is a bug-fix release fixing over 50 issues of a variety of
severities and scopes. A full accounting of these fixes can be found in the
[release notes][]. As always, GHC's release status, including planned future
releases, can be found on the GHC Wiki [status][].

The changes from the second release candidate are:

- Reverting a change the exports of the `Backtrace` constructor in the base library that was backported
   due to confusion on CLC approvals (!14587)
- Reverting a change to the configure script (!14324) that dropped probing for ld.gold

This release candidate will have a two-week testing period. If all goes well
the final release will be available the week of 22 August 2025.

We would like to thank Well-Typed, Tweag I/O, Juspay, QBayLogic, Channable,
Serokell, SimSpace, the Haskell Foundation, and other anonymous contributors
whose on-going financial and in-kind support has facilitated GHC maintenance
and release management over the years. Finally, this release would not have
been possible without the hundreds of open-source contributors whose work
comprise this release.

As always, do give this release a try and open a [ticket][] if you see
anything amiss.


[release notes]: https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.10/docs/users_guide/9.10.3-notes.rst?ref_type=heads&plain=1
[status]: https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC-status
[downloads.haskell.org]: https://downloads.haskell.org/ghc/9.10.3-rc3
[ticket]: https://gitlab.haskell.org/ghc/ghc/-/issues/new
-------------- next part --------------
A non-text attachment was scrubbed...
Name: signature.asc
Type: application/pgp-signature
Size: 488 bytes
Desc: not available
URL: <http://mail.haskell.org/pipermail/haskell-cafe/attachments/20250808/87408248/attachment.sig>

From ietf-dane at dukhovni.org  Fri Aug  8 12:44:26 2025
From: ietf-dane at dukhovni.org (Viktor Dukhovni)
Date: Fri, 8 Aug 2025 22:44:26 +1000
Subject: [Haskell-cafe] GHC 9.10.3-rc3 is now available
In-Reply-To: <jc7mraxljldxz2xp7b6f57yfhimdxladjwjoyzxrykkqqejjsv@g2wojh42lusc>
References: <jc7mraxljldxz2xp7b6f57yfhimdxladjwjoyzxrykkqqejjsv@g2wojh42lusc>
Message-ID: <aJXxKmpCL519syLf@chardros.imrryr.org>

On Fri, Aug 08, 2025 at 06:02:26PM +0530, Zubin Duggal wrote:

> The GHC developers are very pleased to announce the availability
> of the third release candidate for GHC 9.10.3. Binary distributions, source
> distributions, and documentation are available at [downloads.haskell.org][] and
> via [GHCup](https://www.haskell.org/ghcup/).
> 
> GHC 9.10.3 is a bug-fix release fixing over 50 issues of a variety of
> severities and scopes. A full accounting of these fixes can be found in the
> [release notes][]. As always, GHC's release status, including planned future
> releases, can be found on the GHC Wiki [status][].

Is there an associated git tag?  Even after "git fetch origin", all I
see are:

    $ git tag -l | grep 'ghc-9.10\\>'
    ghc-9.10.1-alpha1
    ghc-9.10.1-alpha2
    ghc-9.10.1-alpha3
    ghc-9.10.1-rc1
    ghc-9.10.1-release
    ghc-9.10.2-rc1
    ghc-9.10.2-release
    ghc-9.10.3-rc1
    ghc-9.10.3-rc2

-- 
    Viktor.  ðŸ‡ºðŸ‡¦ Ð¡Ð»Ð°Ð²Ð° Ð£ÐºÑ€Ð°Ñ—Ð½Ñ–!

From zubin at well-typed.com  Mon Aug 18 14:24:55 2025
From: zubin at well-typed.com (Zubin Duggal)
Date: Mon, 18 Aug 2025 19:54:55 +0530
Subject: [Haskell-cafe] GHC 9.10.3-rc4 is now available
Message-ID: <ufsiejujiege6jwm5cxdzqiqsdzox2cwlzzbylc4ibrosnzgbq@ofh6vr5idhlk>

The GHC developers are very pleased to announce the availability
of the fourth release candidate for GHC 9.10.3. Binary distributions, source
distributions, and documentation are available at [downloads.haskell.org][] and
via [GHCup](https://www.haskell.org/ghcup/).

GHC 9.10.3 is a bug-fix release fixing over 50 issues of a variety of
severities and scopes. A full accounting of these fixes can be found in the
[release notes][]. As always, GHC's release status, including planned future
releases, can be found on the GHC Wiki [status][].

The changes from the third release candidate are:

- A fix for a rare segfault with code involving STM (#26205)
- A fix for the `naturalAndNot` returning bogus results (#26205)
- A fix for a crash in the renamer (#25056)

This release candidate will have a two-week testing period. If all goes well
the final release will be available the week of 1 September 2025.

We would like to thank Well-Typed, Tweag I/O, Juspay, QBayLogic, Channable,
Serokell, SimSpace, the Haskell Foundation, and other anonymous contributors
whose on-going financial and in-kind support has facilitated GHC maintenance
and release management over the years. Finally, this release would not have
been possible without the hundreds of open-source contributors whose work
comprise this release.

As always, do give this release a try and open a [ticket][] if you see
anything amiss.


[release notes]: https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.10/docs/users_guide/9.10.3-notes.rst?ref_type=heads&plain=1
[status]: https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC-status
[downloads.haskell.org]: https://downloads.haskell.org/ghc/9.10.3-rc4
[ticket]: https://gitlab.haskell.org/ghc/ghc/-/issues/new

-------------- next part --------------
A non-text attachment was scrubbed...
Name: signature.asc
Type: application/pgp-signature
Size: 488 bytes
Desc: not available
URL: <http://mail.haskell.org/pipermail/haskell-cafe/attachments/20250818/740b303e/attachment.sig>

From andrew.lelechenko at gmail.com  Fri Aug 22 18:09:58 2025
From: andrew.lelechenko at gmail.com (Andrew Lelechenko)
Date: Fri, 22 Aug 2025 19:09:58 +0100
Subject: [Haskell-cafe] Takeover of AvlTree and COrdering
Message-ID: <DA73F8B8-1519-4657-9128-85D802541884@gmail.com>

Iâ€™d like to take over Hackage packages https://hackage.haskell.org/package/COrdering and https://hackage.haskell.org/package/AvlTree. Last uploads were in 2008. Both packages fail to compile with modern GHCs; my intention is to bring them back to life. 

There are no maintainer contacts and no source code repository linked, so I cannot reach out to the maintainer to offer my services. 

Best regards,
Andrew
-------------- next part --------------
An HTML attachment was scrubbed...
URL: <http://mail.haskell.org/pipermail/haskell-cafe/attachments/20250822/d6093615/attachment.html>


"""
