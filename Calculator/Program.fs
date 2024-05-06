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

type Function =
    | Factorial
    | Fibonacci

let rec call (f: Function) (v: int): int = 
    match f with
    | Factorial -> 
        match v with
            | 0 -> 1
            | n when n < 0 -> 0
            | number ->
                let recursion = call f (number - 1)
                number * recursion
    | Fibonacci -> 
        match v with
            | n when n <= 1 -> v
            | v -> 
                let first = call f (v - 1)
                let second = call f (v - 2)
                first + second

type Expression = 
    | Value of int
    | Apply of Operator * Expression * Expression
    | Call of Function * Expression

let rec eval (expression: Expression): int =
    match expression with 
        | Value x -> x
        | Apply (operation, left, right) ->
            let leftEvaluated = eval left
            let rightEvaluated = eval right
            apply operation leftEvaluated rightEvaluated
        | Call (f, expression) -> 
            let evaluatedExpression = eval expression
            call f evaluatedExpression

let average (expressions: List<Expression>): float = 
    if List.isEmpty expressions then
        0
    else 
        List.map eval expressions
        |> List.map float
        |> List.average

let x = Value 3
let y = Value 2
let z = Value 1
let ypz = Apply (Add, y, z)
let full = Apply (Add, x, y)
eval full
|> printfn "%d"

let exprList = [Value 10; Value 20; Value 30]
average exprList
|> printfn "Average = %f"

let exprList2 = []
average exprList2
|> printfn "Average = %f"

call Factorial 4
|> printfn "4! = %d"


call Factorial 0
|> printfn "0! = %d"


call Factorial -4
|> printfn "-4! = %d"

call Fibonacci 8    
|> printfn "8: = %d"