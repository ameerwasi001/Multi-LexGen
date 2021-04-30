module Lexer

open Token
open ParserCombinator

let rec digit = ((((((((((literal_parse "1") ||| (literal_parse "2")) ||| (literal_parse "3")) ||| (literal_parse "4")) ||| (literal_parse "5")) ||| (literal_parse "6")) ||| (literal_parse "7")) ||| (literal_parse "8")) ||| (literal_parse "9")) ||| (literal_parse "0"))
and lowerAlpha = ((((((((((((((((((((((((((literal_parse "a") ||| (literal_parse "b")) ||| (literal_parse "c")) ||| (literal_parse "d")) ||| (literal_parse "e")) ||| (literal_parse "f")) ||| (literal_parse "g")) ||| (literal_parse "h")) ||| (literal_parse "i")) ||| (literal_parse "j")) ||| (literal_parse "k")) ||| (literal_parse "l")) ||| (literal_parse "m")) ||| (literal_parse "n")) ||| (literal_parse "o")) ||| (literal_parse "p")) ||| (literal_parse "q")) ||| (literal_parse "r")) ||| (literal_parse "s")) ||| (literal_parse "t")) ||| (literal_parse "u")) ||| (literal_parse "v")) ||| (literal_parse "w")) ||| (literal_parse "x")) ||| (literal_parse "y")) ||| (literal_parse "z"))
and upperAlpha = ((((((((((((((((((((((((((literal_parse "A") ||| (literal_parse "B")) ||| (literal_parse "C")) ||| (literal_parse "D")) ||| (literal_parse "E")) ||| (literal_parse "F")) ||| (literal_parse "G")) ||| (literal_parse "H")) ||| (literal_parse "I")) ||| (literal_parse "J")) ||| (literal_parse "K")) ||| (literal_parse "L")) ||| (literal_parse "M")) ||| (literal_parse "N")) ||| (literal_parse "O")) ||| (literal_parse "P")) ||| (literal_parse "Q")) ||| (literal_parse "R")) ||| (literal_parse "S")) ||| (literal_parse "T")) ||| (literal_parse "U")) ||| (literal_parse "V")) ||| (literal_parse "W")) ||| (literal_parse "X")) ||| (literal_parse "Y")) ||| (literal_parse "Z"))
and alpha = ((fun _x -> lowerAlpha _x) ||| (fun _x -> upperAlpha _x))
and keyword = (((literal_parse "if") ||| (literal_parse "then")) ||| (literal_parse "else"))
and whitespace = (((literal_parse "\t") ||| (literal_parse "\r")) ||| (literal_parse " "))
and ident = ((fun _x -> alpha _x) >>> (!* ((fun _x -> alpha _x) ||| (fun _x -> digit _x))))
and num = (!+ (fun _x -> digit _x))
and decimal = ((fun _x -> num _x) >>> (!* ((literal_parse ".") >>> (fun _x -> num _x))))

let get_unmodified_toks tokClass (_x: Value) = tokens_from_rules tokClass [( "NUM", ((fun _x -> decimal _x)) ); ( "WHITESPACE", ((fun _x -> whitespace _x)) ); ( "IDENT", ((fun _x -> ident _x)) ); ( "PLUS", ((literal_parse "+")) ); ( "MINUS", ((literal_parse "-")) ); ( "MUL", ((literal_parse "*")) ); ( "DIV", ((literal_parse "/")) )] _x

let get_toks tokClass (_x: string) = 
 match get_unmodified_toks tokClass ([], _x) [] with
 | Ok(xs) -> modify_token_by_condition tokClass xs (Map([("IDENT", [(fun _t -> if (LexerHelpers.fullyApplies(_t) ((fun _x -> keyword _x))) then  (Ok("KEYWORD"))  else  (Ok "")); (fun _t -> if (LexerHelpers.isUpper(_t) (0)) then  (Ok("UPPERIDENT"))  else  (Ok ""))]); ("NUM", [(fun _t -> if (LexerHelpers.contains(_t) (".") (1)) then  (Ok("FLOAT"))  else  (Ok "")); (fun _t -> if (LexerHelpers.contains(_t) (".") (3)) then  (Ok("IPV4"))  else  (Ok "")); (fun _t -> if (LexerHelpers.contains(_t) (".") (0)) then  (Ok("INT"))  else  (Ok "")); (fun _t -> if true then  ((Error("A number must contain either 1, 0, or 3 dots")))  else  (Ok ""))]); ("WHITESPACE", [(fun _t -> if true then  (Ok("_"))  else  (Ok ""))])]))
 | Error(err) -> Error(err)
