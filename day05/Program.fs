let lines =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun l ->
        l.Split(" -> ")
        |> Array.map (fun k -> k.Split(','))
        |> Array.map (fun a -> (int a[0], int a[1])))
    |> Array.map (fun a -> (a[0], a[1]))
    |> Array.toList

// part one
let linePointsHorAndVerOnly = function
    | ((x1, y1), (x2, y2)) when x1 = x2 && y1 <= y2 -> [ for y in y1 .. y2 -> (x1, y) ]
    | ((x1, y1), (x2, y2)) when x1 = x2             -> [ for y in y1 .. -1 .. y2 -> (x1, y) ]
    | ((x1, y1), (x2, y2)) when y1 = y2 && x1 <= x2 -> [ for x in x1 .. x2 -> (x, y1) ]
    | ((x1, y1), (x2, y2)) when y1 = y2             -> [ for x in x1 .. -1 .. x2 -> (x, y1) ]
    | _ -> []

let answer1 =
    lines
    |> List.collect linePointsHorAndVerOnly
    |> List.countBy id
    |> List.filter (fun (_, c) -> c >= 2)
    |> List.length
printfn "Answer part 1: %i" answer1

// part two
let linePointsHorAndVerAndDiag = function
    | ((x1, y1), (x2, y2)) when x1 = x2 || y1 = y2 -> linePointsHorAndVerOnly ((x1, y1), (x2, y2))
    | ((x1, y1), (x2, y2)) ->
        let xrange =
            match (x1, x2) with
            | (x1, x2) when x1 <= x2 -> [ x1 .. x2 ]
            | _                      -> [ x1 .. -1 .. x2 ]
        let yrange =
            match (y1, y2) with
            | (y1, y2) when y1 <= y2 -> [ y1 .. y2 ]
            | _                      -> [ y1 .. -1 .. y2 ]
        List.zip xrange yrange

let answer2 =
    lines
    |> List.collect linePointsHorAndVerAndDiag
    |> List.countBy id
    |> List.filter (fun (_, c) -> c >= 2)
    |> List.length
printfn "Answer part 2: %i" answer2