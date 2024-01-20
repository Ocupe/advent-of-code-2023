app "day-1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [pf.Stdout, pf.Task.{ Task }, pf.Utc, "input.txt" as puzzleInput : Str]
    provides [main] to pf

main =
    start <- Task.await Utc.now

    result =
        puzzleInput
        |> Str.split "\n"
        |> List.keepOks findCalibrationValue
        |> List.sum

    _ <- Task.await (Stdout.line "Result part 1:  \(Num.toStr result)")

    end <- Task.await Utc.now
    duration = end |> Utc.deltaAsMillis start |> Num.toStr
    Stdout.line ("It took \(duration)ms to run day 1 part 1.")

findCalibrationValue = \line ->
    symbols = Str.graphemes line
    firstValue = firstNumber symbols
    lastValue = lastNumber symbols
    "\(Num.toStr firstValue)\(Num.toStr lastValue)" |> Str.toI32

firstNumber = \strList ->
    when strList is
        ["1", ..] ->  1
        ["2", ..] ->  2
        ["3", ..] ->  3
        ["4", ..] ->  4
        ["5", ..] ->  5
        ["6", ..] ->  6
        ["7", ..] ->  7
        ["8", ..] ->  8
        ["9", ..] ->  9
        ["o", "n", "e", ..] ->  1
        ["t", "w", "o", ..] ->  2
        ["t", "h", "r", "e", "e", ..] ->  3
        ["f", "o", "u", "r", ..] ->  4
        ["f", "i", "v", "e", ..] ->  5
        ["s", "i", "x", ..] ->  6
        ["s", "e", "v", "e", "n", ..] ->  7
        ["e", "i", "g", "h", "t", ..] ->  8
        ["n", "i", "n", "e", ..] ->  9
        [] -> crash "No numbers found."
        _ -> (firstNumber (List.dropFirst strList 1))

lastNumber = \strList ->
    when strList is
        [.., "1"] ->  1
        [.., "2"] ->  2
        [.., "3"] ->  3
        [.., "4"] ->  4
        [.., "5"] ->  5
        [.., "6"] ->  6
        [.., "7"] ->  7
        [.., "8"] ->  8
        [.., "9"] ->  9
        [.., "o", "n", "e"] ->  1
        [.., "t", "w", "o"] ->  2
        [.., "t", "h", "r", "e", "e"] ->  3
        [.., "f", "o", "u", "r"] ->  4
        [.., "f", "i", "v", "e"] ->  5
        [.., "s", "i", "x"] ->  6
        [.., "s", "e", "v", "e", "n"] ->  7
        [.., "e", "i", "g", "h", "t"] ->  8
        [.., "n", "i", "n", "e"] ->  9
        [] -> crash "No numbers found."
        _ -> lastNumber (List.dropLast strList 1)
