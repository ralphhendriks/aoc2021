open System
open System.IO

let input = File.ReadAllText("input.txt")

let parseInput (input: string) =
    let s = input.Split(Environment.NewLine + Environment.NewLine)
    let numbers = s[0].Split(',') |> Array.map int |> Array.toList
    let boards =
        s[1..]
        |> Array.map (fun l ->
            l.Split([| " "; Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int)
        |> Array.toList
    (numbers, boards)

let hasWon (n: int Set) (b: int[]) =
    let h1 = n.Contains(b[ 0]) && n.Contains(b[ 1]) && n.Contains(b[ 2]) && n.Contains(b[ 3]) && n.Contains(b[ 4])
    let h2 = n.Contains(b[ 5]) && n.Contains(b[ 6]) && n.Contains(b[ 7]) && n.Contains(b[ 8]) && n.Contains(b[ 9])
    let h3 = n.Contains(b[10]) && n.Contains(b[11]) && n.Contains(b[12]) && n.Contains(b[13]) && n.Contains(b[14])
    let h4 = n.Contains(b[15]) && n.Contains(b[16]) && n.Contains(b[17]) && n.Contains(b[18]) && n.Contains(b[19])
    let h5 = n.Contains(b[20]) && n.Contains(b[21]) && n.Contains(b[22]) && n.Contains(b[23]) && n.Contains(b[24])
    let v1 = n.Contains(b[ 0]) && n.Contains(b[ 5]) && n.Contains(b[10]) && n.Contains(b[15]) && n.Contains(b[20])
    let v2 = n.Contains(b[ 1]) && n.Contains(b[ 6]) && n.Contains(b[11]) && n.Contains(b[16]) && n.Contains(b[21])
    let v3 = n.Contains(b[ 2]) && n.Contains(b[ 7]) && n.Contains(b[12]) && n.Contains(b[17]) && n.Contains(b[22])
    let v4 = n.Contains(b[ 3]) && n.Contains(b[ 8]) && n.Contains(b[13]) && n.Contains(b[18]) && n.Contains(b[23])
    let v5 = n.Contains(b[ 4]) && n.Contains(b[ 9]) && n.Contains(b[14]) && n.Contains(b[19]) && n.Contains(b[24])
    h1 || h2 || h3 || h4 || h5 || v1 || v2 || v3 || v4 || v5

let (numbers, boards) = parseInput input

let rec loop drawn lastDrawn toDraw =
    match List.tryFind (hasWon drawn) boards with
    | None -> loop (drawn.Add(List.head toDraw)) (List.head toDraw) (List.tail toDraw)
    | Some b -> (b, lastDrawn, drawn)
let x = loop Set.empty 0 numbers

let rec loop2 drawn lastDrawn toDraw =
    match List.forAll (not hasWon drawn) boards with
    | None -> loop2 (drawn.Add(List.head toDraw)) (List.head toDraw) (List.tail toDraw)
    | Some b -> (b, lastDrawn, drawn)
let y = loop2 Set.empty 0 numbers

let calculateScore (b, lastDrawn, (drawn: int Set)) =
    let unmarkedSum =
        b
        |> Array.filter (fun n -> not (drawn.Contains n))
        |> Array.reduce (+)
    unmarkedSum * lastDrawn

printfn "Answer part 1: %i" (calculateScore x)
printfn "Answer part 2: %i" (calculateScore y)