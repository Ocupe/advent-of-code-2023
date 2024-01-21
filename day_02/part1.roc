app "day-2-part-1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [pf.Stdout, pf.Task.{ Task }, pf.Utc, "input.txt" as puzzleInput : Str]
    provides [main] to pf

main =
    start <- Task.await Utc.now

    result =
        puzzleInput
        |> Str.split "\n"
        |> List.map parseGame
        |> List.keepIf isGamePossible
        |> List.map .id
        |> List.sum

    _ <- Task.await (Stdout.line "Result part 1:  \(Num.toStr result)")
    end <- Task.await Utc.now
    duration = end |> Utc.deltaAsMillis start |> Num.toStr
    Stdout.line ("It took \(duration)ms to run day 1 part 1.")

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

    # eg: ["3 blue", " 4 red", "3 green"]
    colorStrs
    |> List.walk
        { red: 0, green: 0, blue: 0 }
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

isGamePossible : Game -> Bool
isGamePossible = \{ graps } ->
    # Determine which games would have been possible if the bag had been loaded with only
    # 12 red cubes,
    # 13 green cubes,
    # and 14 blue cubes.
    redCubes = 12
    greenCubes = 13
    blueCubes = 14
    grapsPossible =
        List.keepIf graps (\{ red, green, blue } -> red <= redCubes && green <= greenCubes && blue <= blueCubes)
    (List.len graps) == (List.len grapsPossible)
