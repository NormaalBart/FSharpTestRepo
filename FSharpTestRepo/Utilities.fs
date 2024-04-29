module Utilities

open Types

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

let charOfField (field: Field): char = 
    match field with
    | Empty -> ' '
    | Taken symbol -> charOfSymbol symbol
