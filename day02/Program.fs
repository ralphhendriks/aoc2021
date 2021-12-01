open System.IO

let commands =
    File.ReadAllLines("input.txt")
    |> Array.toList
    |> List.map (fun x -> x.Split(' ') |> (fun l -> (l[0], int l[1])))

// part one
let followCommand (hor, depth) (direction, distance) =
    match direction with
    | "forward" -> (hor + distance, depth)
    | "down" -> (hor, depth + distance)
    | "up" -> (hor, depth - distance)
    | _ -> failwith "command not recognized"

let answer1 =
    commands
    |> List.fold followCommand (0, 0)
    |> (fun (hor, depth) -> hor * depth)
printfn "Answer part 1: %A" answer1

// part two
let followCommandAccordingToManual (hor, depth, aim) (direction, distance) =
    match direction with
    | "down" -> (hor, depth, aim + distance)
    | "up" -> (hor, depth, aim - distance)
    | "forward" -> (hor + distance, depth + distance * aim, aim)
    | _ -> failwith "command not recognized"

let answer2 =
    commands
    |> List.fold followCommandAccordingToManual (0, 0, 0)
    |> (fun (hor, depth, _) -> hor * depth)
printfn "Answer part 2: %A" answer2