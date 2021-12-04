open System.IO

let input =
    File.ReadAllLines("test.txt")

printfn "%A" input