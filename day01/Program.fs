open System.IO

let input =
    File.ReadAllLines("input.txt")
    |> Array.map int
    |> Array.toList

// part one
let numberOfIncreases =
    List.pairwise
    >> List.map (fun (p, n) -> n - p)
    >> List.filter (fun x -> x > 0)
    >> List.length

let answer1 = numberOfIncreases input
printfn "Answer part 1: %i" answer1

// part two
let answer2 =
    input
    |> List.windowed 3
    |> List.map List.sum
    |> numberOfIncreases
printfn "Answer part 2: %i" answer2