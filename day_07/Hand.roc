interface Hand
    exposes [fromList, toList, value, sortHands, handHasJack, handType, improveHand]
    imports []

Hand := List U8 implements [Eq]

HandValue : [
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
]

fromList : List U8 -> Result Hand [InvalidCardCount]
fromList = \hand ->
    when List.len hand is
        5 -> Ok (@Hand hand)
        _ -> Err InvalidCardCount

expect fromList [1, 1, 1, 1, 1] == Ok (@Hand [1, 1, 1, 1, 1])
expect fromList [1, 1, 1, 1] == Err InvalidCardCount

toList : Hand -> List U8
toList = \@Hand list ->
    list

value : Hand -> HandValue
value = \hand ->
    when ((Set.fromList (toList hand) |> Set.len), highestCardCount hand |> .count) is
        (1, 5) -> FiveOfAKind
        (2, 4) -> FourOfAKind
        (2, 3) -> FullHouse
        (3, 3) -> ThreeOfAKind
        (3, 2) -> TwoPair
        (4, 2) -> OnePair
        (5, 1) -> HighCard
        (_, _) -> crash "Invalid hand value."

sortHands : { hand : Hand, betterHand : Hand, bid : U64 }, { hand : Hand, betterHand : Hand, bid : U64 } -> [LT, EQ, GT]
sortHands = \a, b ->
    handValueA = handValue a.betterHand
    handValueB = handValue b.betterHand
    if (handValueA) > (handValueB) then
        LT
    else if (handValueA) == (handValueB) then
        when (a.hand, b.hand) is
            (@Hand handA, @Hand handB) -> cardByCardComparison (handA) (handB)
    else
        GT

cardByCardComparison : List U8, List U8 -> [LT, EQ, GT]
cardByCardComparison = \handA, handB ->
    when (handA, handB) is
        ([a, .. as restA], [b, .. as restB]) ->
            if a > b then
                LT
            else if a < b then
                GT
            else
                cardByCardComparison restA restB

        ([], []) -> EQ
        _ -> crash "Invalid hands"

expect
    actual = cardByCardComparison [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]
    actual == EQ

expect
    actual = cardByCardComparison [1, 2, 3, 5, 5] [1, 2, 3, 4, 5]
    actual == LT

highestCardCount : Hand -> { card : U8, count : U8 }
highestCardCount = \@Hand hand ->
    updatedDict : [Present U8, Missing] -> [Present U8, Missing]
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

getHighestCardInHand : Hand -> U8
getHighestCardInHand = \@Hand hand ->
    hand
    |> List.walk
        0
        \state, card ->
            if card > state then
                card
            else
                state

highestCardCountInList : List U8 -> { card : U8, count : U8 }
highestCardCountInList = \hand ->
    updatedDict : [Present U8, Missing] -> [Present U8, Missing]
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

handValue : Hand -> U8
handValue = \hand ->
    when handType hand is
        FiveOfAKind -> 20
        FourOfAKind -> 19
        FullHouse -> 18
        ThreeOfAKind -> 17
        TwoPair -> 16
        OnePair -> 15
        HighCard -> 14

handHasJack : Hand -> Bool
handHasJack = \hand ->
    when hand is
        @Hand h -> h |> List.contains 1
expect
    actual = handHasJack (@Hand [1, 2, 3, 4, 5])
    actual == Bool.true

jackCountInHand : Hand -> U64
jackCountInHand = \@Hand cards -> cards |> List.countIf \card -> card == 1

expect
    actual = jackCountInHand (@Hand [1, 1, 2, 3, 4])
    actual == 2

# handType : @Hand -> [FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair, HighCard]
handType = \@Hand hand ->
    when ((Set.fromList (hand) |> Set.len), highestCardCount (@Hand hand) |> .count) is
        (1, 5) -> FiveOfAKind
        (2, 4) -> FourOfAKind
        (2, 3) -> FullHouse
        (3, 3) -> ThreeOfAKind
        (3, 2) -> TwoPair
        (4, 2) -> OnePair
        (5, 1) -> HighCard
        (a, b) ->
            dbg hand

            dbg (a, b)

            crash ("Invalid hand")

replaceJackWith : U8, Hand -> Hand
replaceJackWith = \replacementCard, @Hand cards ->
    @Hand (cards |> List.map \card -> if card == 1 then replacementCard else card)

improveHand : Hand -> Hand
improveHand = \hand ->
    cardList = toList hand
    when Hand.handType hand is
        HighCard ->
            highestCard = cardList |> List.sortDesc |> List.first |> Result.withDefault 0
            # OnePair
            replaceJackWith
                highestCard
                hand

        OnePair ->
            when jackCountInHand hand is
                1 ->
                    { card } = highestCardCount hand
                    replaceJackWith card hand

                2 ->
                    highestCard = hand |> getHighestCardInHand
                    replaceJackWith highestCard hand

                _ -> crash "Invalid jack count for OnePair"

        TwoPair ->
            jackCount = cardList |> List.countIf (\card -> card == 1)
            when jackCount is
                1 ->
                    highestCard = cardList |> List.sortDesc |> List.first |> Result.withDefault 0
                    # FullHouse
                    replaceJackWith highestCard hand

                2 ->
                    { card: highestCard } = cardList |> List.keepIf (\card -> card != 1) |> highestCardCountInList
                    # FourOfAKind
                    replaceJackWith highestCard hand

                _ ->
                    handAsStr = cardList |> List.map Num.toStr |> Str.joinWith ", "
                    crash "Unexpected jack count for TwoPair cardList: $(handAsStr)"

        ThreeOfAKind ->
            jackCount = cardList |> List.countIf \card -> card == 1

            when jackCount is
                1 ->
                    { card: highestCard } = cardList |> highestCardCountInList
                    # FourOfAKind
                    replaceJackWith highestCard hand

                3 ->
                    highestCard = cardList |> List.sortDesc |> List.first |> Result.withDefault 0
                    # FourOfAKind
                    replaceJackWith highestCard hand

                _ ->
                    handAsStr = cardList |> List.map Num.toStr |> Str.joinWith ", "
                    crash "Unexpected jack count for ThreeOfAKind cardList: $(handAsStr)"

        FullHouse ->
            notAJack : U8
            notAJack =
                cardList
                |> List.walkUntil
                    1
                    \state, number ->
                        if number != 1 then
                            Break number
                        else
                            Continue state

            when notAJack is
                1 -> crash "This can't be a jack."
                # FiveOfAKind
                a -> replaceJackWith a hand

        FourOfAKind ->
            # TODO: this is the same as the FullHouse branche.
            notAJack : U8
            notAJack =
                cardList
                |> List.walkUntil
                    1
                    \state, number ->
                        if number != 1 then
                            Break number
                        else
                            Continue state

            when notAJack is
                1 -> crash "This can't be a jack."
                # FiveOfAKind
                a -> replaceJackWith a hand

        FiveOfAKind ->
            # FiveOfAKind
            replaceJackWith 13 hand

        _ -> crash "Not supported"

# Improve hands when min one jack is present
# HighCard with 1 Jack -> OnePair
expect
    actual = improveHand (@Hand [1, 2, 3, 4, 5])
    actual == @Hand [5, 2, 3, 4, 5]

# OnePair with 1 indi Jacks -> ThreeOfAKind(replace with pair card)
expect
    actual = improveHand (@Hand [1, 2, 8, 3, 3])
    actual == @Hand [3, 2, 8, 3, 3]
# OnePair with 2 dep Jacks -> OnePair replace with highest card.
expect
    actual = improveHand (@Hand [1, 1, 3, 4, 5])
    actual == @Hand [5, 5, 3, 4, 5]

# TwoPair with 1 independend Jack -> FullHouse
expect
    actual = improveHand (@Hand [2, 2, 1, 3, 3])
    actual == @Hand [2, 2, 3, 3, 3]

# TwoPair with 2 dependend Jacks -> FullHouse (replace Jacks with single card).
expect
    actual = improveHand (@Hand [1, 1, 2, 3, 3])
    actual == @Hand [3, 3, 2, 3, 3]

expect
    actual = improveHand (@Hand [12, 10, 1, 1, 10])
    actual == @Hand [12, 10, 10, 10, 10]

# -
# ThreeOfAKind with 1 indi Jack -> FourOfAKind (replace Jach with three of a kind card)
expect
    actual = improveHand (@Hand [2, 2, 2, 1, 3])
    actual == @Hand [2, 2, 2, 2, 3]
# ThreeOfAKind with 3 dep Jacks -> ThreeOfAKind replace Jacks with highest card.
expect
    actual = improveHand (@Hand [1, 1, 1, 2, 3])
    actual == @Hand [3, 3, 3, 2, 3]
# -
# FullHouse with Jacks -> FiveOfAKind (replace Jacks with oposite cards)
expect
    actual = improveHand (@Hand [1, 1, 1, 2, 2])
    actual == @Hand [2, 2, 2, 2, 2]
# -
# FourOfAKind with jacks -> FiveOfAKind (replace Jack with other card)
expect
    actual = improveHand (@Hand [1, 2, 2, 2, 2])
    actual == @Hand [2, 2, 2, 2, 2]
# -
# FiveOfAKind with 5 Jacks -> FiveOfAKind (replace Jacks with highest card)
expect
    actual = improveHand (@Hand [1, 1, 1, 1, 1])
    actual == @Hand [13, 13, 13, 13, 13]
