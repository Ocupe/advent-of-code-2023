app "day-6-part-1"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.5.2/9VrPjwfQQ1QeSL3CfmWr2Pr9DESdDIXy97pwpuq84Ck.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        # "input.txt" as puzzleInput : Str,
        parser.Core.{ many, Parser, const, skip, keep, chompUntil },
        parser.String.{ parseStr, codeunit, digits, string },
    ]
    provides [main] to pf

day = "6"
part = "1"

testInput =
    """
    Time:      7  15   30
    Distance:  9  40  200
    """

main =
    start <- Task.await Utc.now
    _ <- Task.await (Stdout.line "Run app for day \(day) (part \(part)):")

    _ <- Task.await (Stdout.line " Result:")
    dbg solve testInput

    end <- Task.await Utc.now
    duration = end |> Utc.deltaAsMillis start |> Num.toStr
    Stdout.line "It took \(duration)ms to run day \(day) part \(part).\n"

solve = \input ->
    data =
        parseStr pRaceData input
        |> Result.withDefault [[], []]
        |> \lists ->
            when lists is
                [listA, listB] -> List.map2 listA listB (\a, b -> { time: a, distance: b })
                _ -> []
    data

pSingleNumber =
    const (\n -> n)
    |> skip (many (codeunit ' '))
    |> keep digits

expect
    actual = parseStr pSingleNumber "6"
    actual == Ok 6

pMultipleNumbers =
    const (\ns -> ns)
    |> keep (many pSingleNumber)
    |> skip (many (string "\n"))

expect
    actual = parseStr pMultipleNumbers "6 4 12"
    actual == Ok [6, 4, 12]

expect
    actual = parseStr pMultipleNumbers "6 4 12\n"
    actual == Ok [6, 4, 12]

pRow =
    const (\ns -> ns)
    |> skip (chompUntil ':')
    |> skip (codeunit ':')
    |> skip (many (codeunit ' '))
    |> keep (pMultipleNumbers)

expect
    actual = parseStr pRow "Time:      7  15   30"
    actual == Ok [7, 15, 30]

pRaceData =
    const (\ns -> ns)
    |> keep (many pRow)
    |> skip (many (string "\n"))

expect
    actual = parseStr pRaceData testInput
    actual == Ok [[7, 15, 30], [9, 40, 200]]

