module Lp

open Token
open ParserCombinator

let rec num_s = literal_parse "s" ||| ((fun _x -> n _x) >>> (fun _x -> num_s _x))
and alpha = literal_parse "a" ||| literal_parse "b" ||| literal_parse "c" ||| literal_parse "d"
and n = literal_parse "1" ||| literal_parse "2"
and nx = (!+ (fun _x -> n _x)) >>> !* (literal_parse "." >>> !+ (fun _x -> n _x))
and ident = !+ alpha

let rules = [("NUM", nx); ("IDENT", ident); ("PLUS", literal_parse "+"); ("MINUS", literal_parse "-"); ("SPACE", literal_parse " ")]


let f1 (t: TokenInterface) = if LexerHelpers.contains t "." 1 then Ok "FLOAT" else Ok ""
let f2 (t: TokenInterface) = if LexerHelpers.contains t "." 0 then Ok "INT" else Ok ""
let f3 (_: TokenInterface) = Ok "_"

let get_unmodified_toks tokClass inp = tokens_from_rules tokClass rules inp []
let get_toks tokClass inp = 
    match get_unmodified_toks tokClass inp with
    | Ok(xs) -> modify_token_by_condition tokClass xs (Map([("NUM", [f1; f2]); ("SPACE", [f3])]))
    | Error(err) -> Error err
