open System.IO

let input =
    File.ReadAllLines("input.txt")
    |> Array.toList

// part one
let answer1 =
    let wordLength = input[0].Length
    [ 0 .. wordLength - 1 ]
    |> List.map (fun n ->
        input
        |> List.filter (fun w -> w[wordLength - 1 - n] = '1')
        |> List.length
        |> (fun l -> if l > input.Length - l then (pown 2 n, 0) else (0, pown 2 n)))
    |> List.reduce (fun (p1, q1) (p2, q2) -> (p1 + p2, q1 + q2))
    |> fun (p, q) -> p * q
printfn "Answer part 1: %i" answer1

// part two
let answer2 =
    let rating op =
        let rec loop (numbers: string list) i =
            if numbers.Length = 1 then numbers[0]
            else
                let ones = numbers |> List.filter (fun n -> n[i] = '1')
                let zeros = numbers |> List.filter (fun n -> n[i] = '0')
                match op ones.Length zeros.Length with
                | true -> loop ones (i + 1)
                | false -> loop zeros (i + 1)
        let s = loop input 0
        System.Convert.ToInt32(s, 2)
    let oxygenGeneratorRating = rating (>=)
    let co2ScrubberRating = rating (<)
    oxygenGeneratorRating * co2ScrubberRating
printfn "Answer part 2: %i" answer2