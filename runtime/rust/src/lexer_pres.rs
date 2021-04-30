use lexerGenerator::parserCombinator::*;
use lexerGenerator::tokenType::*;
use lexerGenerator::inline_fun;
use std::collections::HashMap;

pub fn one(_x: Value) -> ResType { literal_parse(String::from("1"), _x) }
pub fn two(_x: Value) -> ResType { literal_parse(String::from("2"), _x) }
pub fn num(_x: Value) -> ResType { unary_op(String::from("+"), |_x|bin_op(one, String::from("|"), two, _x), _x) }

pub fn r(_x: Value) -> ResType { literal_parse(String::from("r"), _x) }
pub fn g(_x: Value) -> ResType { literal_parse(String::from("g"), _x) }
pub fn h(_x: Value) -> ResType { literal_parse(String::from("h"), _x) }
pub fn a(_x: Value) -> ResType { literal_parse(String::from("a"), _x) }
pub fn b(_x: Value) -> ResType { literal_parse(String::from("b"), _x) }
pub fn space(_x: Value) -> ResType { literal_parse(String::from(" "), _x) }
pub fn alpha(_x: Value) -> ResType { bin_op(r, String::from("|"), |_x|bin_op(g, String::from("|"), |_x|bin_op(h, String::from("|"), |_x|bin_op(a, String::from("|"), b, _x), _x), _x), _x) }
pub fn ident(_x: Value) -> ResType { bin_op(alpha, String::from(">"), |_x|unary_op(String::from("*"), |_x|bin_op(num, String::from("|"), alpha, _x), _x), _x) }

pub fn plus(_x: Value) -> ResType { literal_parse(String::from("+"), _x) }
pub fn minus(_x: Value) -> ResType { literal_parse(String::from("-"), _x) }

// fn _f1(_: Token) -> Option<String> { Some(String::from("_")) }
// fn _f2(t: Token) -> Option<String> { if t.1.contains("1") && t.1.contains("2") {Some(String::from("NUM"))} else {None} }
// fn _f3(t: Token) -> Option<String> { if t.1.contains("2") {Some(String::from("2Na"))} else {None} }

pub fn get_unmodified_toks(_x: Value) -> Result<Vec<Token>, String> {
    tokens_from_rules(
        vec![
            (String::from("IDENT"), ident as RuleType),
            (String::from("NUM"), num as RuleType), 
            (String::from("MINUS"), minus as RuleType), 
            (String::from("PLUS"), plus as RuleType),
            (String::from("SPACE"), space as RuleType)
        ], 
        _x
    )
}

pub fn get_toks(_x: Value) -> Result<Vec<Token>, String> {
    let toks_res = get_unmodified_toks(_x);
    match toks_res {
        Ok(toks) => modify_token_by_condition(
            toks, 
            make_map(vec![
                ("SPACE", vec![
                    inline_fun!(t: Token => ModifierRes := Ok(String::from("_")) )
                    ]
                ),
                ("NUM", vec![
                    inline_fun!(t: Token => ModifierRes := if t.1.contains("1") && t.1.contains("2") {Ok(String::from("NUM"))} else {Ok(String::from(""))} ), 
                    inline_fun!(t: Token => ModifierRes := if t.1.contains("2") {Ok(String::from("2Na"))} else {Ok(String::from(""))} )
                    ]
                )
            ])
        ),
        Err(err) => panic!(err)
    }
}