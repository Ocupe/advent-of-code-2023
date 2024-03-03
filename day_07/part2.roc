app "day-6-part-2"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.5.2/9VrPjwfQQ1QeSL3CfmWr2Pr9DESdDIXy97pwpuq84Ck.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        "input.txt" as puzzleInput : Str,
        parser.Core.{ many, Parser, const, skip, oneOf, keep },
        parser.String.{ parseStr, digits, string },
        Hand.{ Hand },
    ]
    provides [main] to pf

day = "7"
part = "2"

testInput =
    """
    32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483
    """

main =
    start <- Task.await Utc.now
    _ <- Task.await (Stdout.line "Run app for day $(day) (part $(part)):")
    dbg solve puzzleInput

    end <- Task.await Utc.now
    duration = end |> Utc.deltaAsMillis start |> Num.toStr
    Stdout.line "It took $(duration)ms to run day $(day) part $(part).\n"

solve = \input ->
    input
    |> Str.split "\n"
    |> List.keepOks
        (\line -> parseStr pLine line)
    |> List.map
        \{ hand, bid } ->
            if Hand.handHasJack hand then
                { hand, betterHand: Hand.improveHand hand, bid }
            else
                { hand, betterHand: hand, bid }
    |> List.sortWith Hand.sortHands
    |> List.reverse
    |> List.walkWithIndex
        0
        (\state, elem, index ->
            state + (elem.bid * (index + 1))
        )

expect
    actual = solve testInput
    actual == 5905

# A, K, Q, T, 9, 8, 7, 6, 5, 4, 3, 2, J
pCard : Parser _ U8
pCard =
    oneOf [
        const 13 |> skip (string "A"),
        const 12 |> skip (string "K"),
        const 11 |> skip (string "Q"),
        const 10 |> skip (string "T"),
        const 9 |> skip (string "9"),
        const 8 |> skip (string "8"),
        const 7 |> skip (string "7"),
        const 6 |> skip (string "6"),
        const 5 |> skip (string "5"),
        const 4 |> skip (string "4"),
        const 3 |> skip (string "3"),
        const 2 |> skip (string "2"),
        const 1 |> skip (string "J"),
    ]

expect
    actual = parseStr pCard "A"
    actual == Ok 13

pHand : Parser _ (List U8)
pHand =
    many pCard

expect
    actual = parseStr pHand "32T3K"
    actual == Ok [3, 2, 10, 3, 12]

pBid : Parser _ U64
pBid =
    const (\n -> n)
    |> keep (digits)
    |> skip (many (string "\n"))

expect
    actual = parseStr pBid "765\n"
    actual == Ok 765

pLine : Parser _ { hand : Hand, bid : U64 }
pLine =
    const
        (\hand -> \bid ->
                when Hand.fromList hand is
                    Ok handResult -> { hand: handResult, bid }
                    Err InvalidCardCount -> crash "Invalid card count. I need exactly 5 cards."
        )
    |> keep (pHand)
    |> skip (string " ")
    |> keep (pBid)

