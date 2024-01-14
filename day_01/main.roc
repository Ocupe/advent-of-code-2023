app "day-1"
    packages  { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [pf.Stdout, pf.Task.{ Task  }, pf.Utc, "input" as puzzleInput : Str]
    provides [main] to pf

main =
    start <- Task.await Utc.now

    result = puzzleInput 
        |> Str.split "\n" 
        |> List.keepOks solvePuzzle
        |> List.sum
        |> Num.toStr
    _ <- Task.await (Stdout.line "Result part 1:  \(result)")

    end <- Task.await Utc.now
    duration = end |> Utc.deltaAsMillis start |> Num.toStr
    Stdout.line ("It took \(duration)ms to run day 1 part 1.")

solvePuzzle = \input -> 
    numbers = 
        input
        |> Str.graphemes
        |> ( \symboles -> List.keepOks symboles Str.toI32 )
        |> List.map Num.toStr

    when (List.first numbers, List.last numbers) is
        (Ok first, Ok last) -> "\(first)\(last)" |> Str.toI32
        _ -> crash "List was empty"



    


