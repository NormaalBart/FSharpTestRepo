type Operator = 
    | Add
    | Subtract
    | Multiply
    | Divide

let apply (o: Operator) (left: int) (right: int): int =
    match o with
        | Add -> left + right
        | Subtract -> left - right
        | Multiply -> left * right
        | Divide -> left / right

type Expression = 
    | Value of int
    | Apply of Operator * Expression * Expression

let rec eval (expression: Expression): int =
    match expression with 
        | Value x -> x
        | Apply (operation, left, right) ->
            let le = eval left
            let re = eval right
            apply operation le re

let x = Value 3
let y = Value 2
let z = Value 1
let ypz = Apply (Add, y, z)
let full = Apply (Multiply, x, ypz)
eval full
|> printfn "%d"
