app "day-5-part-2"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        "input.txt" as puzzleInput : Str,
    ]
    provides [main] to pf

day = "5"
part = "2"

testInput =
    """
    seeds: 79 14 55 13

    seed-to-soil map:
    50 98 2
    52 50 48

    soil-to-fertilizer map:
    0 15 37
    37 52 2
    39 0 15

    fertilizer-to-water map:
    49 53 8
    0 11 42
    42 0 7
    57 7 4

    water-to-light map:
    88 18 7
    18 25 70

    light-to-temperature map:
    45 77 23
    81 45 19
    68 64 13

    temperature-to-humidity map:
    0 69 1
    1 0 69

    humidity-to-location map:
    60 56 37
    56 93 4
    """

main =
    start <- Task.await Utc.now
    _ <- Task.await (Stdout.line "Run app for day \(day) (part \(part)):")

    _ <- Task.await (Stdout.line " Result:")
    dbg solve puzzleInput

    end <- Task.await Utc.now
    duration = end |> Utc.deltaAsMillis start |> Num.toStr
    Stdout.line "It took \(duration)ms to run day \(day) part \(part).\n"

solve = \input ->
    paragraphs = input |> Str.split "\n\n"

    seeds =
        paragraphs
        |> List.first
        |> \result ->
            when result is
                Ok seedsRow -> seedsRow
                Err ListWasEmpty -> crash "Expected first row to exist - list was empty"
        |> parseSeedRow
        |> expandToSeedRange

    maps =
        paragraphs
        |> List.map paragraphToMap

    seeds
    |> List.map \seed -> resolveSeedToLocation seed maps
    |> List.min
    |> \result ->
        when result is
            Ok lowest -> lowest
            Err ListWasEmpty -> crash "List was empty."

Map : { name : Str, ranges : List { dest : U64, src : U64, span : U64 } }

resolveSeedToLocation : U64, List Map -> U64
resolveSeedToLocation = \seed, maps ->
    List.walk
        maps
        seed
        (\state, map ->
            List.walkUntil
                map.ranges
                state
                (\innerState, range ->
                    if range.src <= innerState && innerState <= range.src + range.span then
                        # Do mapping:
                        # x - range.src = delta
                        # range.dest + delta = mapped
                        mapped = innerState |> \x -> x - range.src |> \delta -> range.dest + delta
                        Break mapped
                    else
                        # Out of mapping range
                        Continue innerState

                )
        )

expect
    seeds =
        testInput
        |> Str.split "\n\n"
        |> List.first
        |> \result ->
            when result is
                Ok seedsRow -> seedsRow
                Err ListWasEmpty -> crash "Expected first row to exist - list was empty"
        |> parseSeedRow

    seeds == [79, 14, 55, 13]

expect
    paragraph =
        """
        seed-to-soil map:
        50 98 2
        52 50 48
        """
    maps = paragraph |> paragraphToMap

    maps == { name: "seed-to-soil map:", ranges: [{ dest: 50, src: 98, span: 2 }, { dest: 52, src: 50, span: 48 }] }

paragraphToMap = \paragraph ->
    rows =
        paragraph
        |> Str.split "\n"
    name =
        rows
        |> List.first
        |> \result ->
            when result is
                Ok first -> first
                Err ListWasEmpty -> crash "I expect the first row to exist - list was empty."
    ranges =
        rows
        |> List.dropFirst 1
        |> List.map (\row -> Str.split row " ")
        |> List.map (\strNumbers -> List.keepOks strNumbers Str.toU64)
        |> List.map
            (\numbers ->
                when numbers is
                    [a, b, c] -> { dest: a, src: b, span: c }
                    _ -> crash "I expected a list with three items."
            )
    { name, ranges }

parseSeedRow = \str ->
    str
    |> Str.splitFirst ":"
    |> \result ->
        when result is
            Ok { before: "seeds", after } -> after
            Err NotFound -> crash "String is missing : to split on."
            _ -> crash "Invalid seeds row."
    |> Str.trim
    |> Str.split " "
    |> List.keepOks Str.toU64

expect
    parseSeedRow "seeds: 79 14 55 13" == [79, 14, 55, 13]

expandToSeedRange : List U64 -> List U64
expandToSeedRange = \list ->
    seedRanges =
        list
        |> List.chunksOf 2

    initList : List U64
    initList = []

    expand : List U64, List U64 -> List U64
    expand = \state, range ->
        when range is
            [a, b] ->
                state
                |> List.concat
                    (
                        List.range { start: At (Num.toNat a), end: Length (Num.toNat b) }
                        |> List.map Num.toU64
                    )

            _ -> crash "Expected a list of pairs."

    seedRanges
    |> List.walk
        initList
        expand

expect
    expandToSeedRange [10, 2, 14, 4] == [10, 11, 14, 15, 16, 17]

