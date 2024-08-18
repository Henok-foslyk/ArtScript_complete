
open Evaluator
open System
open Parser

[<EntryPoint>]
let main args =

    let file = args[0]
    let text = IO.File.ReadAllText file
    match parse text with
    | Some ast ->
        let svg = eval ast
        printfn "%s" svg
        0
    | None ->
        printfn "Invalid program. Check possible keyword syntax error."
        1
