open System

type Symbol =
    | Cross
    | Nought

type FieldIndex = 
    | First
    | Second
    | Third

type FieldPosition = {
    RowIndex: FieldIndex;
    ColIndex: FieldIndex;
}

let indexToEnum i =
    match i with
    | 0 -> Some First
    | 1 -> Some Second
    | 2 -> Some Third
    | _ -> None

let charOfSymbol (symbol: Symbol): char =
    match symbol with
    | Cross -> 'X'
    | Nought -> 'O'

type Field =
    | Empty
    | Taken of Symbol

let charOfField (field: Field): char = 
    match field with
    | Empty -> ' '
    | Taken symbol -> charOfSymbol symbol

type Row = Field * Field * Field
type Board = Row * Row * Row

let emptyRow = (Field.Empty, Field.Empty, Field.Empty)
let initialBoard: Board = (emptyRow, emptyRow, emptyRow)

let printRow (row: Row) =
    let (f1, f2, f3) = row
    printfn "|%c|%c|%c|" (charOfField f1) (charOfField f2) (charOfField f3)

let printBoard (board: Board) =
    let (row1, row2, row3) = board
    printRow row1
    printfn "-------"
    printRow row2
    printfn "-------"
    printRow row3

let checkRow (r: Row) =
    match r with
    | (Taken s1, Taken s2, Taken s3) when s1 = s2 && s2 = s3 -> Some s1
    | _ -> None

let checkColumn (board: Board) (index: int) =
    let (row1, row2, row3) = board
    match (index, row1, row2, row3) with
    | (0, (Taken s1, _, _), (Taken s2, _, _), (Taken s3, _, _))
    | (1, (_, Taken s1, _), (_, Taken s2, _), (_, Taken s3, _))
    | (2, (_, _, Taken s1), (_, _, Taken s2), (_, _, Taken s3)) when s1 = s2 && s2 = s3 -> Some s1
    | _ -> None

let checkDiagonal (board: Board) =
    let (row1, row2, row3) = board
    match (row1, row2, row3) with
    | ((Taken s1, _, _), (_, Taken s2, _), (_, _, Taken s3))
    | ((_, _, Taken s1), (_, Taken s2, _), (Taken s3, _, _)) when s1 = s2 && s2 = s3 -> Some s1
    | _ -> None

let findWinner (board: Board) =
    let (row1, row2, row3) = board
    [ checkRow row1; checkRow row2; checkRow row3;
      checkColumn board 0; checkColumn board 1; checkColumn board 2;
      checkDiagonal board ]
    |> List.choose id
    |> List.tryHead    

let moveIndexToPosition (move: int) : Option<FieldPosition> =
    if move < 1 || move > 9 then None
    else
        let rowIndex = (move - 1) / 3
        let colIndex = (move - 1) % 3
        match indexToEnum rowIndex, indexToEnum colIndex with
        | Some r, Some c -> Some { RowIndex = r; ColIndex = c }
        | _ -> None


let isBoardFull (board: Board) = 
    let (row1, row2, row3) = board
    let squares = [row1; row2; row3] |> List.collect (fun (s1, s2, s3) -> [s1; s2; s3])
    squares |> List.forall (fun square ->
        match square with
        | Empty -> false
        | Taken _ -> true)

let updateBoard (board: Board) (position: FieldPosition) (symbol: Symbol) : Board =
    let (row1, row2, row3) = board
    let rows = [row1; row2; row3]

    let updateRow (f1, f2, f3) col =
        match col with
        | First -> (Taken symbol, f2, f3)
        | Second -> (f1, Taken symbol, f3)
        | Third -> (f1, f2, Taken symbol)

    let updatedRows = 
        rows |> List.mapi (fun i r -> 
            if i = (match position.RowIndex with | First -> 0 | Second -> 1 | Third -> 2) then 
                updateRow r position.ColIndex 
            else r)
    (updatedRows.[0], updatedRows.[1], updatedRows.[2])

let isValidMove (board: Board) (position: FieldPosition) : bool =
    let (row1, row2, row3) = board
    let rows = [row1; row2; row3]
    let rowIndex = match position.RowIndex with
                   | First -> 0
                   | Second -> 1
                   | Third -> 2
    let colIndex = match position.ColIndex with
                   | First -> 0
                   | Second -> 1
                   | Third -> 2
    let (f1, f2, f3) = rows.[rowIndex]
    let field : Option<Field> = match colIndex with
                                | 0 -> Some f1
                                | 1 -> Some f2
                                | 2 -> Some f3
                                | _ -> None
    match field with
    | Some Empty -> true
    | Some (Taken _) -> false
    | None -> false

let rec playGame (board: Board) (turn: Symbol) =
    printBoard board
    let winner = findWinner board
    match winner with
    | Some s ->
        printfn "Player %c wins!" (charOfSymbol s)
    | None ->
        if isBoardFull board then
            printfn "Draw game!"
        else
            printf "Player %c's turn. Enter a move (1-9): " (charOfSymbol turn)
            let input = Console.ReadLine()
            match Int32.TryParse(input) with
            | (true, move) ->
                match moveIndexToPosition move with
                | Some position ->
                    if isValidMove board position then
                        let updatedBoard = updateBoard board position turn
                        let nextTurn = if turn = Cross then Nought else Cross
                        playGame updatedBoard nextTurn
                    else
                        printfn "Invalid move. Please try again."
                        playGame board turn
                | None ->
                    printfn "Invalid move. Please try again."
                    playGame board turn
            | _ -> 
                printfn "Invalid input. Please enter a number between 1 and 9."
                playGame board turn

playGame initialBoard Symbol.Cross