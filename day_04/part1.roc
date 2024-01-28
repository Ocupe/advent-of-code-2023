app "day-4-part-1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        # "input.txt" as puzzleInput : Str,
    ]
    provides [main] to pf

day = "4"
part = "1"

testInput =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """

main =
    start <- Task.await Utc.now
    _ <- Task.await (Stdout.line "Run app for day \(day) (part \(part)):")

    _ <- Task.await (Stdout.line " Result:")
    dbg solve testInput

    end <- Task.await Utc.now
    duration = end |> Utc.deltaAsMillis start |> Num.toStr
    Stdout.line ("It took \(duration)ms to run day \(day) part \(part).\n")

solve = \input ->
    input
    |> Str.split "\n"
    |> List.map parseRow

Card : { id : Str, winningNumbers : List U32, numbers : List U32 }

parseRow : Str -> Card
parseRow = \str ->
    str
    |> Str.splitFirst ":"
    |> (\result ->
        when result is
            Ok { before, after } ->
                id = parseId before
                { winningNumbers, numbers } = parseNumberBlock after
                { id: id, winningNumbers: winningNumbers, numbers: numbers }

            Err NotFound -> crash "Every row should have a ':'"
    )

expect (parseRow "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53") == { id: "1", winningNumbers: [41, 48, 83, 86, 17], numbers: [83, 86, 6, 31, 17, 9, 48, 53] }

parseId : Str -> Str
parseId = \str ->
    str
    |> Str.split " "
    |> List.last
    |> (\result ->
        when result is
            Ok id -> id
            Err ListWasEmpty -> crash "Every row should have an id"
    )

expect (parseId "Card 1") == "1"

parseNumberBlock : Str -> { winningNumbers : List U32, numbers : List U32 }
parseNumberBlock = \str ->
    str
    |> Str.splitFirst "|"
    |> (\result ->
        when result is
            Ok { before, after } ->
                { winningNumbers: parseNumbers before, numbers: parseNumbers after }

            Err NotFound -> crash "Every row should have a | separator."
    )

expect (parseNumberBlock "41 48 83 86 17 | 83 86  6 31 17  9 48 53") == { winningNumbers: [41, 48, 83, 86, 17], numbers: [83, 86, 6, 31, 17, 9, 48, 53] }

parseNumbers : Str -> List U32
parseNumbers = \str ->
    str
    |> Str.split " "
    |> List.keepIf (\item -> item != "")
    |> List.map Str.trim
    |> List.map Str.toU32
    |> List.map
        (\result ->
            when result is
                Ok numbers -> numbers
                Err InvalidNumStr -> crash "Could not parse the number."
        )

expect (parseNumbers "83 86  6 31 17  9 48 53") == [83, 86, 6, 31, 17, 9, 48, 53]

cardValue : Card -> Num.U32
cardValue = \card ->
    winningNumbers = Set.fromList card.winningNumbers
    numbers = Set.fromList card.numbers

    elements = Set.intersection winningNumbers numbers |> Set.toList
    dbg winningNumbers

    List.walk
        elements
        0
        (\state, _ ->
            if state == 0 then
                1
            else
                state * 2
        )

expect (cardValue { id: "1", winningNumbers: [41, 48, 83, 86, 17], numbers: [83, 86, 6, 31, 17, 9, 48, 53] }) == 8
