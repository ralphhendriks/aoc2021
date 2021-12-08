let positions =
    System.IO.File.ReadAllText("input.txt")
    |> (fun s -> s.Split(','))
    |> Array.map int
    |> Array.toList
 
let minimumFuel =
    [(List.min positions) .. (List.max positions)]
    |> List.map (fun r ->
        positions
        |> List.map (fun y -> abs (y - r))
        |> List.sum)
    |> List.min

printfn "%A" minimumFuel

let minimumFuel2 =
    [(List.min positions) .. (List.max positions)]
    |> List.map (fun r ->
        positions
        |> List.map (fun y ->
            let d = abs (y - r)
            d * (d + 1) / 2)
        |> List.sum)
    |> List.min

let linearFuelRate x y = abs (x - y)

let increasingFuelRate x y =
    let d = abs (x - y)
    d * (d + 1) / 2

let findMinimumFuel fuelAlgorithm =
    [(List.min positions) .. (List.max positions)]
    |> List.map (fun r ->
        positions
        |> List.map (fun p -> fuelAlgorithm r p)
        |> List.sum)
    |> List.min

printfn "%A" minimumFuel2
printfn "Answer part 1: %i" (findMinimumFuel linearFuelRate)
printfn "Answer part 2: %i" (findMinimumFuel increasingFuelRate)