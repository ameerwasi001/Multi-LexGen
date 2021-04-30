use crate::tokenType;
use std::collections::HashMap;

pub type ErrResType = Result<Value, (String, String)>;
pub type Value = (Vec<String>, String);
pub type ResType = Result<Value, String>;
pub type FunctionRes = fn(Value) -> Result<Value, String>;
pub type RuleType = fn(Value) -> ResType;
pub type ModifierRes = Result<String, String>;
pub type ModifierType = fn(tokenType::Token) -> ModifierRes;

pub fn fst(tuple: (Vec<String>, String)) -> Vec<String> {
    match tuple {
        (a, _) => a
    }
}

pub fn snd(tuple: (Vec<String>, String)) -> String {
    match tuple {
        (_, b) => b
    }
}

fn concat_vec<A>(a: Vec<A>, b: Vec<A>) -> Vec<A> where A: Clone {
    vec![a, b].concat()
}

pub fn literal_parse(expected: String, inp: Value) -> ResType {
    let mut match_str = String::from("");
    let mut i = 0;
    let (first, mut second) = inp;
    if expected.len() > second.len() {
        return Err(String::from(format!("InputError: Cannot match {} with {}", second, expected)))
    };
    while i<expected.len(){
        let character = second.remove(0);
        match_str = format!("{}{}", match_str, character);
        i += 1;
    };
    if match_str == expected { Ok((concat_vec(first, vec![match_str]), second)) } 
    else { Err(String::from(format!("InputError: expected {}, found {}", expected, match_str))) }
}

pub fn bin_op<A, B>(left: A, op: String, right: B, inp: Value) -> ResType 
    where A: Fn(Value) -> Result<Value, String>,
    B: Fn(Value) -> Result<Value, String> {
    if op == "|" {
        match left(inp.clone()) {
            Ok(a) => return Ok(a),
            Err(_) => match right(inp) {
                Ok(a) => return Ok(a),
                Err(err) => return Err(err)
            }
        }
    } else if op == ">" {
        match left(inp) {
            Ok(a) => return right(a),
            Err(err) => return Err(err)
        }
    }
    return Err(String::from(format!("This should not have happend, I expected {} but got {}", "| or >", op)))
}

pub fn unary_op<F>(op: String, operand: F, inp: Value) -> ResType where F: Fn(Value) -> Result<Value, String> {
    if op == "*" || op == "+" {
        let mut output = operand(inp.clone());
        match output {
            Ok(_) => (),
            Err(err) => return if op == "*" {Ok(inp)} else {Err(err)}
        };
        loop {
            match operand(output.clone().unwrap()) {
                Ok(a) => {
                    output = Ok(a)
                    
                },
                Err(_) => return output
            }
        };
    } else if op == "?" {
        let output = operand(inp.clone());
        match output {
            Ok(a) => return Ok(a),
            Err(_) => return Ok(inp)
        }
    };
    panic!("Can't understand postfix operator, {}", op)
}

pub fn tokens_from_rules(ruleset: Vec<(String, RuleType)>, inp: Value) -> Result<Vec<tokenType::Token>, String> {
        let mut generated_toks: Vec<tokenType::Token> = vec![];
        let mut next_inp = inp.clone();
        while snd(next_inp.clone()) != "" {
            let mut i = 0;
            let mut matched = false;
            while i < ruleset.len() {
                let (tok, rule) = &ruleset[i];
                match rule((vec![], snd(next_inp.clone()))) {
                    Ok(a) => {
                        next_inp = a;
                        matched = true
                    },
                    Err(_) => {
                        i += 1;
                        continue
                    }
                };
                let x = tokenType::Token(String::from(tok), fst(next_inp.clone()).join(""));
                generated_toks.push(x);
                i += 1;
            };
            if !matched {
                let (_, snd) = next_inp;
                let sndlines: Vec<&str> = snd.lines().into_iter().collect();
                let text = if snd.chars().nth(0).unwrap() == '\n' {sndlines[1]} else {sndlines[0]};
                return Err(String::from(format!("Unmatched text {}", text)))
            }
        };
        return Ok(generated_toks)
}

pub fn modify_token_by_condition(toks: Vec<tokenType::Token>, modfiers: HashMap<String, Vec<ModifierType>>) -> Result<Vec<tokenType::Token>, String> {
    let mut moded_toks = vec![];
    for tok in toks {
        let mods = modfiers.get(&tok.0);
        let mut new_tok = tok.clone();
        let mut should_add = true;
        match mods {
            None => (),
            Some(mods) => {
                for modifier in mods {
                    match modifier(new_tok.clone()) {
                        Err(err) => return Err(err),
                        Ok(res) if res != "" => {
                            if res == "_" {should_add = false};
                            new_tok = tokenType::Token(res, new_tok.1);
                            break;
                        },
                        Ok(_) => ()
                    }
                }
            }
        }
        if should_add {moded_toks.push(new_tok)}
    };
    Ok(moded_toks)
}


pub fn make_map(vector: Vec<(&str, Vec<ModifierType>)>) -> HashMap<String, Vec<ModifierType>> {
    let mut map: HashMap<String, Vec<ModifierType>> = HashMap::new();
    for (k, v) in vector {
        map.insert(k.to_string(), v);
    }
    map
}
