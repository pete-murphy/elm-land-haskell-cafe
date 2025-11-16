## Current behavior

Currently, the UI renders stuff that looks like

<current-ui>
2025-August.txt
From: ICFP Publicity
Subject: [Haskell-cafe] ICFP 2025 (Co-Located with SPLASH!): Call for Participation
Message-ID: CAAmpXihPo=+pyBtQQqjNjGYYwR1yyiPjFaAuGNHui48E_K-2hg@mail.gmail.com
In-Reply-To: <no in-reply-to>
References: <no references>
Date: 8/3/254 months ago
Content:
==================================================
Call for Participation

The 30th ACM SIGPLAN Inte
From: Stefan Klinger
Subject: [Haskell-cafe] Correct parsers for bounded integral values
Message-ID: aI-1wAcOCYOFthbp@tauhou
In-Reply-To: <aHOZt6DINagivTzE@tauhou>
References: <aHOZt6DINagivTzE@tauhou>
Date: 8/3/253 months ago
Content:
Pierre Thierry (2025-Jul-21, excerpt):
> Well, IIUC, it conforms to the Haskell 98 specification.

H
From: Akhra Gannon
Subject: [Haskell-cafe] Correct parsers for bounded integral values
Message-ID: CAJoPsuA0yVW1dnv+ue+qjogMBE65pmeYbB_FQ2KWtS0-xLkM9g@mail.gmail.com
In-Reply-To: <aI-1wAcOCYOFthbp@tauhou>
References: <aHOZt6DINagivTzE@tauhou> <aI-1wAcOCYOFthbp@tauhou>
Date: 8/3/253 months ago
Content:
I believe it's not that integers must wrap, but that Read behavior must
match compiler behavior (and
From: Pierre Thierry
Subject: [Haskell-cafe] Correct parsers for bounded integral values
Message-ID: C41ABEC2-35C6-4708-8445-26F1C00F1C1D@nothos.net
In-Reply-To: <aI-1wAcOCYOFthbp@tauhou>
References: <aHOZt6DINagivTzE@tauhou> <aI-1wAcOCYOFthbp@tauhou>
Date: 8/3/253 months ago
Content:
On August 3, 2025 9:17:20 PM GMT+02:00, Stefan Klinger <haskell at stefan-klinger.de> wrote:
>> Well
From: Zubin Duggal
Subject: [Haskell-cafe] GHC 9.10.3-rc2 is now available
Message-ID: 2pvhnbxa5lneok2e3ohskt3tcntqpoclxmptdk2qoquack5ovy@ofgaqxjmynll
In-Reply-To: <no in-reply-to>
References: <no references>
Date: 8/6/253 months ago
Content:
The GHC developers are very pleased to announce the availability
of the second release candidate for
</current-ui>

## New plan

### Store in IndexedDB (via PGLite)

We want to use PGLite (https://pglite.dev/docs/) to store those `Message`s in indexeddb, instead of just showing them in the browser. Let's create an Elm port that does just that.

<question>Note that the "in-reply-to" field of each message marks the messge's "parent": it would be cool to use `ltree` as a way of indexing messages in the postgres database, what do you think? or do people use recursive CTE's for this instead? what would be preferable here? are either of those even compatible with PGLite?</question>

### Report when done

So, we're removing the current content from the UI, _but_ I'd still like to show something in the UI: it can be used to track progress of fetching these various txt files, and loading them into indexeddb (if this can take time, let's await the mutation, and then send a port msg _back_ to Elm when finished)

## General reference

elm-land docs: https://elm.land/
we particularly want to reference the guide to using Ports in this section: https://elm.land/guide/working-with-js.html