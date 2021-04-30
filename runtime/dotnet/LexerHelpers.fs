module LexerHelpers
open Token
open ParserCombinator
open System.Text.RegularExpressions
open System

let contains (tok: TokenInterface) str times = Regex.Matches(tok.value, Regex.Escape str).Count = times
let isUpper (tok: TokenInterface) i = Char.IsUpper(tok.value.[i])
let fullyApplies (tok: TokenInterface) (rule: Value -> FunRes) = 
    match rule ([], tok.value) with
    | Ok((_, "")) -> true
    | Ok(_) -> false
    | Error(_) -> false
let applies (tok: TokenInterface) (rule: Value -> FunRes) = 
    match rule ([], tok.value) with
    | Ok(_) -> true
    | Error(_) -> false