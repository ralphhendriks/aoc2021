// Player 1 starting position: 4
//                             ^ <-- index 28
let input =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun s -> (s.Substring(28) |> int))

type Player = Player1 | Player2

type GameState =
    { ActivePlayer: Player
      PositionPlayer1: int
      PositionPlayer2: int
      ScorePlayer1: int
      ScorePlayer2: int }

type DeterministicDie =
    { Next: int
      NumberOfTimesRolled: int }

let rollDeterministicDie die =
    (die.Next, { Next = (if die.Next < 100 then die.Next + 1 else 1); NumberOfTimesRolled = die.NumberOfTimesRolled + 1 })

let rollThreeTimes die =
    let (res1, die1) = rollDeterministicDie die
    let (res2, die2) = rollDeterministicDie die1
    let (res3, die3) = rollDeterministicDie die2
    (res1 + res2 + res3, die3)

let initialGame =
    { ActivePlayer = Player1
      PositionPlayer1 = input[0]
      PositionPlayer2 = input[1]
      ScorePlayer1 = 0
      ScorePlayer2 = 0 }

let advancePawn currentSpace roll =
    let x = (currentSpace + roll) % 10
    if x = 0 then 10 else x

let playRoundDeterministic game die =
    let (roll, nextDie) = rollThreeTimes die
    let nextState =
        match game.ActivePlayer with
        | Player1 ->
            let nextSpace = advancePawn game.PositionPlayer1 roll
            {game with ActivePlayer = Player2; PositionPlayer1 = nextSpace; ScorePlayer1 = game.ScorePlayer1 + nextSpace}
        | Player2 ->
            let nextSpace = advancePawn game.PositionPlayer2 roll
            {game with ActivePlayer = Player1; PositionPlayer2 = nextSpace; ScorePlayer2 = game.ScorePlayer2 + nextSpace}
    (nextState, nextDie)

let answer1 =
    let rec loop game die =
        match game.ScorePlayer1, game.ScorePlayer2 with
        | s1, s2 when s1 >= 1000 -> s2 * die.NumberOfTimesRolled
        | s1, s2 when s2 >= 1000 -> s1 * die.NumberOfTimesRolled
        | _ ->
            let (nextGame, nextDie) = playRoundDeterministic game die
            loop nextGame nextDie
    loop initialGame { Next = 1; NumberOfTimesRolled = 0 }

printfn "Answer part 1: %i" answer1

let allRolls =
    List.allPairs [1; 2; 3] [1; 2; 3]
    |> List.allPairs [1; 2; 3]
    |> List.map (fun (a, (b, c)) -> a + b + c)
    |> List.countBy id
    |> List.map (fun (r, n) -> (r, int64 n))

// part 2
let answer2 =
    let rec loop game =
        match game.ScorePlayer1, game.ScorePlayer2 with
        | s1, _ when s1 >= 21 -> (1L, 0L)
        | _, s2 when s2 >= 21 -> (0L, 1L)
        | _ ->
            match game.ActivePlayer with
            | Player1 ->
                allRolls
                |> List.map (fun (r, n) ->
                    let nextSpace = advancePawn game.PositionPlayer1 r
                    loop { game with ActivePlayer = Player2; PositionPlayer1 = nextSpace; ScorePlayer1 = game.ScorePlayer1 + nextSpace }
                    |> (fun (a, b) -> (a * n, b * n))
                )
                |> List.reduce (fun (a, b) (c, d) -> a+c, b+d)
            | Player2 ->
                allRolls
                |> List.map (fun (r, n) ->
                    let nextSpace = advancePawn game.PositionPlayer2 r
                    loop { game with ActivePlayer = Player1; PositionPlayer2 = nextSpace; ScorePlayer2 = game.ScorePlayer2 + nextSpace }
                    |> (fun (a, b) -> (a * n, b * n))
                )
                |> List.reduce (fun (a, b) (c, d) -> a+c, b+d)
    let max (a, b) = max a b
    loop initialGame |> max

printfn "Answer part 2: %i" answer2