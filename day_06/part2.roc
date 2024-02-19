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

    dbg ("Result", solve puzzleInput)

    end <- Task.await Utc.now
    duration = end |> Utc.deltaAsMillis start |> Num.toStr
    Stdout.line "It took \(duration)ms to run day \(day) part \(part).\n"

solve = \input ->
    data =
        parseStr pRaceData input
        |> Result.withDefault [[], []]
        |> List.map \list ->
            joinedNr = list |> List.map Num.toStr |> Str.joinWith "" |> Str.toU64 |> Result.withDefault 0
            [joinedNr]
        |> \lists ->
            when lists is
                [listA, listB] -> List.map2 listA listB (\a, b -> { time: a, distance: b })
                _ -> []
    data
    |> List.map waysToWinTheRace
    |> List.walk 1 (\state, x -> state * x)

expect
    actual = solve testInput
    actual == 71503

waysToWinTheRace = \{ time, distance } ->
    List.range { start: At 1, end: At time }
    |> List.walk
        0
        (\state, pushTime ->
            raceTime = time - pushTime
            speed = pushTime * 1
            raceDistance = raceTime * speed

            if raceDistance > distance then
                state + 1
            else
                state
        )

expect
    actual = waysToWinTheRace { time: 7, distance: 9 }
    actual == 4

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

