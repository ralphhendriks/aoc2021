open System.IO

let counted =
    File.ReadAllText("input.txt")
    |> (fun s -> s.Split(','))
    |> Array.map int
    |> Array.countBy id
    |> Array.map (fun (t, c) -> (t, int64 c))

let step (x: (int * int64)[]) =
    x
    |> Array.collect (fun (t, c) ->
        match t with
        | 0 -> [|(6, c); (8, c)|]
        | t -> [|(t - 1, c)|])
    |> Array.groupBy fst
    |> Array.map (fun (c, l) -> (c, (Array.sumBy snd l)))

let repeat n = Seq.init n (fun _ -> step) |> Seq.reduce (>>)

counted |> repeat 80 |> Array.sumBy snd |> printfn "Answer part 1: %i"
counted |> repeat 256 |> Array.sumBy snd |> printfn "Answer part 2: %i"