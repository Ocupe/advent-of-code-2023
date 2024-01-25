app "day-2-part-1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [pf.Stdout, pf.Task.{ Task }, pf.Utc, "input.txt" as puzzleInput : Str]
    provides [main] to pf

day = "2"
part = "1"

main =
    start <- Task.await Utc.now
    _ <- Task.await (Stdout.line "Run app for day \(day) (part \(part)):")
    grid =
        exampleInput
        |> Str.split "\n"
        |> parseGrid

    # |> List.sum
    # _ <- Task.await (Stdout.line " Result: \(Num.toStr result)")
    end <- Task.await Utc.now
    duration = end |> Utc.deltaAsMillis start |> Num.toStr
    Stdout.line ("It took \(duration)ms to run day \(day) part \(part).\n")

DictKey : {
    row : Num.U16,
    col : Num.U16,
}

Field : [
    Number { utf8 : Num.U8, str : Str },
    Empty,
    Symbol,
]

Grid : Dict DictKey Field

parseGrid = \rows ->
    grid = Dict.empty {}

    List.walkWithIndex
        rows
        grid
        (\state, row, rowIndex ->
            row
            |> Str.toUtf8
            |> List.walkWithIndex
                state
                (\stateCopy, col, colIndex ->
                    field : Field
                    field =
                        when col is
                            49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 -> Number { utf8: col, str: Result.withDefault (Str.fromUtf8 [col]) "ERROR" }
                            46 -> Empty
                            _ -> Symbol
                    Dict.insert stateCopy { row: rowIndex, col: colIndex } field
                )

        )

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

expect exampleInput |> Str.split "\n" |> parseGrid |> Dict.len == 100
