use lexerGenerator::tokenType::*;
use lexerGenerator::parserCombinator::*;

pub fn contains(t: Token, string: String, times: i64) -> bool { t.1.matches(&string.to_owned()).count() == times as usize }
pub fn isUpper(t: Token, i: i64) -> bool { ('A' .. 'Z').contains(&(t.1.as_bytes()[i as usize] as char)) }
pub fn fullyApplies(t: Token, rule: RuleType) -> bool {
    match rule((vec![], t.1))  {
        Ok((_, s)) => s == "",
        Err(_) => false
    }
}
pub fn applies(t: Token, rule: RuleType) -> bool {
    match rule((vec![], t.1))  {
        Ok(_) => true,
        Err(_) => false
    }
}