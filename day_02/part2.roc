app "day-2-part-2"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [pf.Stdout, pf.Task.{ Task }, pf.Utc, "input.txt" as puzzleInput : Str]
    provides [main] to pf

day = "2"
part = "2"

main =
    start <- Task.await Utc.now
    _ <- Task.await (Stdout.line "Run app for day \(day) (part \(part)):")
    result =
        puzzleInput
        |> Str.split "\n"
        |> List.map parseGame
        |> List.map cubesRequiredToPlayGame
        |> List.map \{ red, green, blue } -> (red * green * blue)
        |> List.sum

    _ <- Task.await (Stdout.line " Result: \(Num.toStr result)")
    end <- Task.await Utc.now
    duration = end |> Utc.deltaAsMillis start |> Num.toStr
    Stdout.line ("It took \(duration)ms to run day \(day) part \(part).\n")

Game : {
    id : Num.U16,
    graps : List Grap,
}

Grap : {
    red : Num.U16,
    green : Num.U16,
    blue : Num.U16,
}

parseGame : Str -> Game
parseGame = \line ->
    gameAndGraps = Result.withDefault (Str.splitFirst line ":") { before: "Game 0", after: "" }

    gameId =
        gameAndGraps
        |> .before
        |> (\gameName -> Str.splitFirst gameName " ")
        |> (\split -> Result.withDefault split { before: "Game", after: "0" })
        |> .after
        |> Str.toU16
        |> Result.withDefault 0

    graps : List Grap
    graps =
        gameAndGraps
        |> .after # "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
        |> Str.split ";"
        |> List.map parseGrap

    { id: gameId, graps: graps }

parseGrap : Str -> Grap
parseGrap = \inputStr ->
    # inputStr = "3 blue, 4 red"
    colorStrs = inputStr |> Str.split ","

    toCount : List Str -> Num.U16
    toCount = \graphemes ->
        graphemes
        |> Str.joinWith ""
        |> Str.trim
        |> Str.toU16
        |> Result.withDefault 0

    defaultGrap : Grap
    defaultGrap = { red: 0, green: 0, blue: 0 }

    # 3 blue
    # 4 red
    colorStrs
    |> List.walk
        defaultGrap
        (\state, color ->
            when Str.graphemes color is
                [.. as nr, "b", "l", "u", "e"] ->
                    { state & blue: (state.blue + (toCount nr)) }

                [.. as nr, "r", "e", "d"] ->
                    { state & red: (state.red + (toCount nr)) }

                [.. as nr, "g", "r", "e", "e", "n"] ->
                    { state & green: (state.green + (toCount nr)) }

                _ -> crash "Unknown color (should be blue, red or green)"
        )

cubesRequiredToPlayGame : Game -> Grap
cubesRequiredToPlayGame = \{ graps } ->
    graps
    |> List.walk
        { red: 0, green: 0, blue: 0 }
        (\state, { red, green, blue } -> {
            red: Num.max state.red red,
            green: Num.max state.green green,
            blue: Num.max state.blue blue,
        }
        )

expect cubesRequiredToPlayGame { id: 0, graps: [{ red: 4, green: 0, blue: 3 }, { red: 1, green: 2, blue: 6 }, { red: 0, green: 2, blue: 0 }] } == { red: 4, green: 2, blue: 6 }
