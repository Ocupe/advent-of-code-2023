app "day-2-part-1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [pf.Stdout, pf.Task.{ Task }, pf.Utc, "input.txt" as puzzleInput : Str]
    provides [main] to pf

day = "2"
part = "1"

main =
    start <- Task.await Utc.now
    _ <- Task.await (Stdout.line "Run app for day \(day) (part \(part)):")

    result = solve puzzleInput

    _ <- Task.await (Stdout.line " Result: \(Num.toStr result)")
    end <- Task.await Utc.now
    duration = end |> Utc.deltaAsMillis start |> Num.toStr
    Stdout.line ("It took \(duration)ms to run day \(day) part \(part).\n")

solve = \input ->
    grid =
        input
        |> to2dArray

    symboles = List.walkWithIndex
        grid
        []
        (\state, row, rowIndex ->
            List.walkWithIndex
                row
                state
                (\innerState, field, colIndex ->
                    when field is
                        Symbole ->
                            List.append innerState (rowIndex, colIndex)

                        _ -> innerState
                )
        )
    # dbg symboles

    partialNumbers =
        symboles
        |> List.map (\(row, col) -> findNumbersAdjacentToSymbol grid (row, col))
        |> List.join
    # dbg partialNumbers

    expandedNumbers =
        partialNumbers
        |> List.map (\hit -> expandVertically grid hit)
        |> List.keepOks Str.fromUtf8
        |> List.keepOks Str.toU32
        |> Set.fromList
        |> Set.toList
    # dbg expandedNumbers

    expandedNumbers |> List.sum

Field : [Number Num.U8, Symbole, Empty]

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
        _ -> Symbole

# findNumbersAdjacentToSymbol : Grid, (Num.U8, Num.U8) -> List (Num.U8, Num.U8)
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
                                Number value -> List.append state ({ row: probe.row, col: probe.col })
                                _ -> state

                        Err OutOfBounds -> crash "Field not found in row."
                )
            )

    dbg (row, col)

    dbg suroundingNumbersFields

    # |> List.keepIf
    #     (\field ->
    #         when field is
    #             Number _ -> Bool.true
    #             _ -> Bool.false
    #     )
    suroundingNumbersFields

expandVertically = \grid, { row: rowIndex, col: hitCol } ->
    row = List.get grid rowIndex |> Result.withDefault []
    numberList = List.walkWithIndexUntil
        row
        []
        (\state, field, currentCol ->
            if currentCol <= hitCol then
                # Before hit
                when field is
                    Number value -> Continue (List.append state value)
                    _ -> Continue []
            else
                # After hit
                when field is
                    Number value -> Continue (List.append state value)
                    _ -> Break state

        )
    dbg numberList

    numberList

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
