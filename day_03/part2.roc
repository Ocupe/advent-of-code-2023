app "day-3-part-2"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        "input.txt" as puzzleInput : Str,
    ]
    provides [main] to pf

day = "3"
part = "2"

main =
    start <- Task.await Utc.now
    _ <- Task.await (Stdout.line "Run app for day \(day) (part \(part)):")

    result = solve puzzleInput

    _ <- Task.await (Stdout.line " Result: \(Num.toStr result)")
    end <- Task.await Utc.now
    duration = end |> Utc.deltaAsMillis start |> Num.toStr
    Stdout.line ("It took \(duration)ms to run day \(day) part \(part).\n")

exampleInput =
    """
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """

solve = \input ->
    grid =
        input
        |> to2dArray

    gears = List.walkWithIndex
        grid
        []
        (\state, row, rowIndex ->
            List.walkWithIndex
                row
                state
                (\innerState, field, colIndex ->
                    when field is
                        Gear ->
                            List.append innerState (rowIndex, colIndex)

                        _ -> innerState
                )
        )

    numberHits =
        gears
        |> List.map
            (\(row, col) -> findNumbersAdjacentToSymbol grid (row, col))
        |> List.map (\listOfNeigbors -> List.map listOfNeigbors (\hit -> numberFromHit grid hit))
        |> List.map Set.fromList # Remove duplicates
        |> List.map Set.toList
        |> List.map
            (\list ->
                if (List.len list) == 2 then list else []) # Include only gears with 2 adjacent numbers.

    sumGearRatios =
        numberHits
        |> List.map
            (\pair ->
                when pair is
                    [a, b] -> a.value * b.value
                    [] -> 0
                    _ -> crash "List is not a pair.")
        |> List.sum

    sumGearRatios

Field : [Number Num.U8, Gear, Symbole, Empty]

Grid : List (List Field)

to2dArray : Str -> Grid
to2dArray = \input ->
    input
    |> Str.split "\n"
    |> List.map (\rowStr -> Str.toUtf8 rowStr)
    |> List.map (\row -> List.map row parseField)

parseField : Num.U8 -> Field
parseField = \value ->
    when value is
        48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 -> Number value
        46 -> Empty # 46 is .
        42 -> Gear # 42 is *
        _ -> Symbole

findNumbersAdjacentToSymbol = \grid, (row, col) ->
    suroundingFields = [
        { row: row - 1, col: col - 1 },
        { row: row - 1, col: col },
        { row: row - 1, col: col + 1 },
        { row: row, col: col - 1 },
        { row: row, col: col + 1 },
        { row: row + 1, col: col - 1 },
        { row: row + 1, col: col },
        { row: row + 1, col: col + 1 },
    ]

    suroundingNumbersFields =
        suroundingFields
        |> List.walk
            []
            (\state, probe ->
                grid
                |> List.get probe.row
                |> (\maybeRow ->
                    when maybeRow is
                        Ok selectedRow -> selectedRow
                        Err OutOfBounds -> crash "Row not found in grid."
                )
                |> List.get probe.col
                |> (\maybeField ->
                    when maybeField is
                        Ok selectedField ->
                            when selectedField is
                                Number _ -> List.append state ({ row: probe.row, col: probe.col })
                                _ -> state

                        Err OutOfBounds -> crash "Field not found in row."
                )
            )

    suroundingNumbersFields

numberFromHit = \grid, hit ->
    row = List.get grid hit.row |> Result.withDefault []
    range =
        List.walkWithIndexUntil
            row
            []
            (\state, field, currentCol ->
                if currentCol <= hit.col then
                    # Before hit
                    when field is
                        # Todo: get the start and end column values from the hit
                        Number _ -> Continue (List.append state currentCol)
                        _ -> Continue []
                else
                    # After hit
                    when field is
                        Number _ -> Continue (List.append state currentCol)
                        _ -> Break state

            )

    start = List.first range |> Result.withDefault 0
    end = List.last range |> Result.withDefault 0
    count = end - start + 1

    number =
        row
        |> List.map
            (\field ->
                when field is
                    Number value -> value
                    _ -> 0
            )
        |> Str.fromUtf8Range { start, count }
        |> Result.withDefault "0"
        |> Str.toU32
        |> Result.withDefault 0
        |> (\numList -> { value: numList, row: hit.row, cols: range })

    # dbg { start, end}
    number
