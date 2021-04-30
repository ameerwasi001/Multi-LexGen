module RustGen where

import CodeGen ( CodeGenerator(..), compile, buildMap )
import Debug.Trace ( trace )
import qualified Data.Map as Map
import Data.List
import Nodes

data Rust = Rust

splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _   []  subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise            = split' sub (tail str) (head str:subacc) acc

importRuntime :: String
importRuntime = "use lexerGenerator::parserCombinator::*;\nuse lexerGenerator::tokenType::*;\nuse lexerGenerator::inline_function_macro;\nuse lexerGenerator::inline_fun;\nuse std::collections::HashMap;"

compileExpr :: Expr -> String 
compileExpr (ParseStringNode str _) = "|_x|literal_parse(String::from(" ++ show str ++ "), _x)"
compileExpr (ParseRuleNode id _) = "|_x|" ++ id ++ "(_x)"
compileExpr (ParseBinOpNode a op b _) = "|_x|bin_op(" ++ compileExpr a ++ ", String::from(\"" ++ op ++ "\"), " ++ compileExpr b ++ ", _x)"
compileExpr (ParseUnaryNode op e _) = "|_x|unary_op(String::from(\"" ++ op ++ "\"), " ++ compileExpr e ++ ", _x)"

compileProgramExpr :: String -> ProgramExpr -> String
compileProgramExpr _ (NumNode n _) = n
compileProgramExpr _ (IdentifierNode id _) = id
compileProgramExpr s (StrNode str _) = "String::from(" ++ show str ++ ")"
compileProgramExpr s (CallNode str ids _) = s ++ "::" ++ str ++ "(_t, " ++ intercalate ", " (map (compileProgramExpr s) ids) ++ ")"
compileProgramExpr _ (BoolNode b _) = showL b
compileProgramExpr s (IfNode c t e _) = "if " ++ compileProgramExpr s c ++ (
    case t of 
        Right a -> " { " ++ compileProgramExpr s a ++ " }"
        Left a -> " { Ok(String::from(" ++ show a  ++ ")) }"
    ) ++  " else " ++ maybe "{ Ok(String::from(\"\")) }" (\x -> "{ " ++ show x ++ " }") e
compileProgramExpr _ (RuleProgramNode r _) = compileExpr r
compileProgramExpr _ (EraseNode _) = show "_"
compileProgramExpr _ (ErrorNode str _) = "Err(String::from(" ++ show str ++ "))"

compileRule :: RuleDef -> String 
compileRule (RuleDef id expr _) = "pub fn " ++ id ++ "(_x: Value) -> ResType { " ++ removeFirst4 (compileExpr expr) ++ " }"

removeFirst4 :: [a] -> [a]
removeFirst4 = tail. tail . tail . tail

compileExtractors :: [DataExtractor] -> String
compileExtractors ds = "pub fn get_unmodified_toks(_x: Value) -> Result<Vec<Token>, String> { tokens_from_rules(vec![" ++ intercalate ", " (map (\(DataExtractor id exp _) -> "( String::from(" ++ show id ++ "), (" ++ compileExpr exp ++ ") as RuleType )") ds) ++ "], _x) }"

compileModifiers :: String -> [DataModifier] -> String
compileModifiers supportCode mods = 
    "pub fn get_toks(_x: String) -> Result<Vec<Token>, String> { \n\t" ++
        "match get_unmodified_toks((vec![], _x)) { \n\t\t" ++
            "Ok(a) => modify_token_by_condition(a, make_map(vec![" ++ intercalate ", " (Map.foldrWithKey (\k v ks -> ("(" ++ show k ++ ", " ++ v ++ ")"):ks) [] $ Map.map (\xs -> "vec![" ++ intercalate ", " (map (\x -> "inline_fun!(_t: Token => ModifierRes := " ++ compileProgramExpr supportCode x ++ ") as ModifierType") xs) ++ "]") (buildMap Map.empty mods)) ++ "])),\n\t\t" ++
            "Err(err) => return Err(err)\n\t" ++
        "}\n" ++ 
    "}"

compileLexeme :: String -> Lexeme -> String
compileLexeme supportCode (Lexeme rds exts mods)  = importRuntime ++ "\nuse " ++ supportCode ++ ";\n\n" ++ intercalate "\n" (map compileRule rds) ++ "\n\n" ++ compileExtractors exts ++ "\n\n" ++ compileModifiers (last $ splitStr "::" supportCode) mods

instance CodeGenerator Rust where
    generate _ = compileLexeme