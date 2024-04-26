type Symbol =
    | Cross
    | Nought

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

let isBoardFull (board: Board) = 
    let (row1, row2, row3) = board
    let squares = [row1; row2; row3] |> List.collect (fun (s1, s2, s3) -> [s1; s2; s3])
    squares |> List.forall (fun square ->
        match square with
        | Empty -> false
        | Taken _ -> true)

let playGame (board: Board) (turn: Symbol) =
    printBoard board
    let winner = findWinner board
    if winner.IsSome then 
        printfn "Speler %c wint" (charOfSymbol winner.Value)
    elif isBoardFull board then
        printfn "Gelijkspel, helaas jullie zijn veeeel te goed"
    else
        failwith "todo"



playGame initialBoard Symbol.Cross
