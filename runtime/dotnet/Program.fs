open System
open FSharp.Core
open System.Text.RegularExpressions
open Token
open ParserCombinator

let tok_to_string = List.map (fun (a: TokenInterface) -> a.ToString())

[<EntryPoint>]
let main _ =
    match Lexer.get_toks newDefaultToken "if b then iffer+0+mx+9.2-192.168.0.1/1.2-MxC*oz2 else 0.0.0.0+72.98*10-PI" with
    | Ok(xs) -> List.iter (fun (a: TokenInterface) -> printfn "%s" (a.ToString())) xs
    | Error(err) -> printfn "Error: %A" err
    0 // return an integer exit code
