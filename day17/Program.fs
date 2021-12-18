// Active pattern for regex
let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

// coordinates of the target area
let (xt_min, xt_max, yt_min, yt_max) =
    System.IO.File.ReadAllText("input.txt")
    |> function
    | Regex @"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)" [ xt_min; xt_max; yt_min; yt_max ] ->
        (int xt_min, int xt_max, int yt_min, int yt_max)
    | _ -> failwith "Invalid input"

// part 1
// Studying the trajectory of the example, we can conclude that for an initial vertical velocity vy0 > 0 we are back at y = 0 with vy = -vy0 - 1.
// The heighest altitude can then be reached when we reach the lowest point of the target yt_min in a single step, thus vy0_max = -yt_min - 1
// This only holds if the x-range of the target contains at least one element of the range 0, 1, 3, 6, 10, etc. We assume this to be true for both the example and the actual input.
// Using Gauss' method of summing consecutive numbers we can then infer: y_max = (vy0_max + vy0_max ^ 2) / 2
let answer1 =
    let vy0_max = -yt_min - 1
    (vy0_max + vy0_max * vy0_max)/2

printfn "Answer part 1: %i" answer1

// part 2
let rec hitsTarget (x, y, vx, vy) =
    match (x, y) with
    | (x, y) when x >= xt_min && x <= xt_max && y >= yt_min && y <= yt_max -> true
    | (x, y) when x > xt_max || y < yt_min -> false
    | (x, y) -> hitsTarget (x + vx, y + vy, (if vx > 0 then vx - 1 else 0), vy - 1)

// This optimization only works when yt_max < 0
// The idea is to short-circuit the part of the trajectory where y > 0
// We immediately calculate the starting state at the downward crossing of y = 0 instead of iterating
let optimize (x, y, vx, vy) =
    if vy <= 0 then (x, y, vx, vy)
    else
        let steps = (2 * vy) + 1
        let rec loop n x vx =
            if (vx = 0 || n = 0) then (x, vx)
            else loop (n - 1) (x + vx) (vx - 1)
        let (x1, vx1) = loop steps x vx
        (x1, 0, vx1, -vy - 1)

// try all possible velocity vectors.
// vx0 is between 1 and xt_max
// vy0 is between yt_min and vy0_max (answer1)
let answer2 =
    seq { for vx in 1 .. xt_max do for vy in yt_min .. answer1 do (0, 0, vx, vy) }
    |> Seq.map optimize // comment out to disable the optimization
    |> Seq.filter hitsTarget
    |> Seq.length
printfn "Answer part 2: %i" answer2