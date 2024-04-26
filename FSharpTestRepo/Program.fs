type Symbol =
    | Cross
    | Nought

let stringOfSymbol symbol =
    match symbol with
    | Cross -> "X"
    | Nought -> "O"

printf "%s" (stringOfSymbol Symbol.Cross)


type Field =
    | Empty
    | Taken of Symbol