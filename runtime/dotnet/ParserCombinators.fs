module ParserCombinator
open FSharp.Core
open Token

type Value = string list * string
type FunRes = Result<string list * string, string>
type ModifierRes = Result<string, string>
type ModifierType = TokenInterface -> Result<string, string>

let rec sequenceResult xs =
    let f x xs =
        match x with
        | Ok(a) -> 
            match sequenceResult xs with
            | Ok(b) -> Ok([a]@b)
            | Error(err) -> Error err
        | Error(err) -> Error err
    match xs with
    | [] -> Ok([])
    | x::[] -> f x []
    | x::xs -> f x xs

let newDefaultToken t v = DefaultToken(t, v) :> TokenInterface

let removeChars (s: string) n = s.[n..]
let getChars (s: string) n = s.[..(n-1)]

let literal_parse (expected: string) (res: string list, rest: string) = 
    if expected.Length > rest.Length then Error(sprintf "Cannot match %s with %s" expected rest)
    else if rest.StartsWith(expected) then Ok((res@[getChars rest expected.Length], removeChars rest expected.Length))
    else Error(sprintf "Cannot match %s with %s" expected rest)

let (>>>) lf rf a = 
    match lf a with
    | Ok(a) -> rf a
    | Error(err) -> Error(err)

let (|||) lf rf a =
    match lf a with
    | Ok(a) -> Ok(a)
    | Error(_) -> rf a

let rec (!*) f (a: Value) = 
    match f a with
    | Ok(a) -> !* f a 
    | Error(_) -> Ok(a)

let (!+) f = f >>> !* f 

let (!?) f (inp: Value) =
    match f inp with
    | Ok(a) -> Ok a
    | Error(_) -> Ok inp

let rec choice (rs: (string * (Value -> FunRes)) list) inp = 
    match rs with
    | (n, r)::rs -> 
        match r inp with
        | Ok((res, rest)) -> Ok((n, res, rest))
        | Error(_) -> choice rs inp
    | [] -> 
        let sndlines = snd(inp).Split [|'\n'|]
        Error(sprintf "Unmatched \"%s\"" (if snd(inp).[0] = '\n' then sndlines.[1] else sndlines.[0]))

let rec tokens_from_rules (tokCreate: string -> string -> TokenInterface) (ruleset: (string * (Value -> FunRes)) list) (inp: Value) toks =
    match (inp, toks) with
    | ((_, ""),_) -> Ok(toks)
    | (inp,_) -> 
        match choice ruleset inp with
        | Ok((n, res, rest)) -> 
            tokens_from_rules tokCreate ruleset ([], rest) (toks@[tokCreate n (List.fold (+) "" res)])
        | Error(err) -> Error(err)

let rec applyRules createToken (maybeRules: ModifierType list option) (tok: TokenInterface) = 
    match maybeRules with
    | Some([]) -> Ok tok
    | Some(f::fs) -> 
        match f tok with
        | Ok(a) -> if a = "" then applyRules createToken (Some fs) tok else Ok (createToken a tok.value)
        | Error(err) -> Error(err)
    | None -> Ok tok

let modify_token_by_condition createToken (toks: TokenInterface list) (mods: Map<string, ModifierType list>) = 
    match sequenceResult (List.map (fun (a: TokenInterface) -> applyRules createToken (mods.TryFind a.type_) a) toks) with
    | Ok(xs) -> Ok(List.filter (fun (a: TokenInterface) -> a.type_ <> "_") xs)
    | Error(err) -> Error(err)