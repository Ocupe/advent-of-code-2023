app "day-6-part-1"
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
    ]
    provides [main] to pf

day = "7"
part = "1"

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
    _ <- Task.await (Stdout.line "Run app for day \(day) (part \(part)):")
    dbg solve puzzleInput

    end <- Task.await Utc.now
    duration = end |> Utc.deltaAsMillis start |> Num.toStr
    Stdout.line "It took \(duration)ms to run day \(day) part \(part).\n"

solve = \input ->
    input
    |> Str.split "\n"
    |> List.keepOks
        (\line -> parseStr pLine line)
    |> List.sortWith \a, b -> sortHands a.hand b.hand
    |> List.reverse
    |> List.map
        (\hand ->
            dbg hand

            hand)
    |> List.walkWithIndex
        0
        (\state, elem, index ->
            state + (elem.bid * (index + 1))
        )

sortHands : HandType, HandType -> [LT, EQ, GT]
sortHands = \a, b ->
    handValueA = handValue a
    handValueB = handValue b
    if (handValueA) > (handValueB) then
        LT
    else if (handValueA) == (handValueB) then
        handA = getHand a
        handB = getHand b
        compareHands handA handB
    else
        GT

expect
    actual = List.sortWith [HighCard [2, 3, 4, 5, 6], HighCard [2, 3, 4, 5, 10]] sortHands
    actual == [HighCard [2, 3, 4, 5, 10], HighCard [2, 3, 4, 5, 6]]

expect
    actual = solve testInput
    actual == 6440

# A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2
pCard : Parser _ Num.U8
pCard =
    oneOf [
        const 14 |> skip (string "A"),
        const 13 |> skip (string "K"),
        const 12 |> skip (string "Q"),
        const 11 |> skip (string "J"),
        const 10 |> skip (string "T"),
        const 9 |> skip (string "9"),
        const 8 |> skip (string "8"),
        const 7 |> skip (string "7"),
        const 6 |> skip (string "6"),
        const 5 |> skip (string "5"),
        const 4 |> skip (string "4"),
        const 3 |> skip (string "3"),
        const 2 |> skip (string "2"),
    ]

expect
    actual = parseStr pCard "A"
    actual == Ok 14

Hand : List Num.U8

pHand : Parser _ Hand
pHand =
    many pCard

expect
    actual = parseStr pHand "32T3K"
    actual == Ok [3, 2, 10, 3, 13]

pBid : Parser _ Num.U64
pBid =
    const (\n -> n)
    |> keep (digits)
    |> skip (many (string "\n"))

expect
    actual = parseStr pBid "765\n"
    actual == Ok 765

pLine =
    const (\hand -> \bid -> { hand: handType hand, bid })
    |> keep (pHand)
    |> skip (string " ")
    |> keep (pBid)

expect
    actual = parseStr pLine "32T5K 765"
    actual == Ok { hand: HighCard [3, 2, 10, 5, 13], bid: 765 }

HandType : [
    FiveOfAKind Hand,
    FourOfAKind Hand,
    FullHouse Hand,
    ThreeOfAKind Hand,
    TwoPair Hand,
    OnePair Hand,
    HighCard Hand,
]

handType = \hand ->
    when ((Set.fromList hand |> Set.len), highestCardCount hand |> .count) is
        (1, 5) -> FiveOfAKind hand
        (2, 4) -> FourOfAKind hand
        (2, 3) -> FullHouse hand
        (3, 3) -> ThreeOfAKind hand
        (3, 2) -> TwoPair hand
        (4, 2) -> OnePair hand
        (5, 1) -> HighCard hand
        (a, b) ->
            dbg hand

            dbg (a, b)

            crash ("Invalid hand")

highestCardCount : List Num.U8 -> { card : Num.U8, count : Num.U8 }
highestCardCount = \hand ->
    updatedDict : [Present Num.U8, Missing] -> [Present Num.U8, Missing]
    updatedDict = \maybeNumber ->
        when maybeNumber is
            Missing -> Present 1u8
            Present number -> Present (number + 1u8)

    hand
    |> List.walk
        (Dict.withCapacity 5)
        (\state, card ->
            Dict.update state card updatedDict
        )
    |> Dict.toList
    |> List.sortWith
        (\a, b ->
            if a.1 > b.1 then
                LT
            else if a.1 < b.1 then
                GT
            else
                EQ)
    |> List.first
    |> \maybeFirst ->
        when maybeFirst is
            Ok pair -> { card: pair.0, count: pair.1 }
            Err ListWasEmpty -> crash "No cards in hand"

expect
    actual = highestCardCount [1, 1, 1, 1, 1]
    actual == { card: 1, count: 5 }

expect
    actual = highestCardCount [2, 1, 1, 2, 1]
    actual == { card: 1, count: 3 }

expect
    actual = handType [1, 1, 1, 1, 1]
    actual == FiveOfAKind [1, 1, 1, 1, 1]

expect
    actual = handType [1, 1, 1, 1, 2]
    actual == FourOfAKind [1, 1, 1, 1, 2]

expect
    actual = handType [1, 1, 1, 2, 2]
    actual == FullHouse [1, 1, 1, 2, 2]

expect
    actual = handType [3, 1, 2, 5, 4]
    actual == HighCard [3, 1, 2, 5, 4]

getHand : HandType -> Hand
getHand = \hand ->
    when hand is
        FiveOfAKind h -> h
        FourOfAKind h -> h
        FullHouse h -> h
        ThreeOfAKind h -> h
        TwoPair h -> h
        OnePair h -> h
        HighCard h -> h

compareHands : Hand, Hand -> [LT, EQ, GT]
compareHands = \handA, handB ->
    when (handA, handB) is
        ([a, .. as restA], [b, .. as restB]) ->
            if a > b then
                LT
            else if a < b then
                GT
            else
                compareHands restA restB

        ([], []) -> EQ
        _ -> crash "Invalid hands"

expect
    actual = compareHands [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]
    actual == EQ

expect
    actual = compareHands [1, 2, 3, 5, 5] [1, 2, 3, 4, 5]
    actual == LT

handValue : HandType -> Num.U8
handValue = \hand ->
    when hand is
        FiveOfAKind _ -> 20
        FourOfAKind _ -> 19
        FullHouse _ -> 18
        ThreeOfAKind _ -> 17
        TwoPair _ -> 16
        OnePair _ -> 15
        HighCard _ -> 14
