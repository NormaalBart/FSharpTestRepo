open System
open Types
open GameLogic

[<EntryPoint>]
let main argv =
    let rec wantToRestartGame (): bool =
        let answer = Console.ReadLine()
        match answer.ToLower() with
        | "y" | "yes" -> 
            true
        | "n" | "no" -> 
            false
        | _ -> 
            printfn "Invalid input, y/n expected"
            wantToRestartGame()

    let rec startGame () =
        printfn "Starting Tic-Tac-Toe game. Cross goes first!"
        playGame initialBoard Symbol.Cross
        printfn "Do you want to play again? (y/n)"
        match wantToRestartGame () with
        | true -> 
            startGame()
        | false -> 
            printfn "Thanks for playing!"

    startGame ()
    0
