module FSharpGen where

import CodeGen ( CodeGenerator(..), compile, buildMap )
import Debug.Trace ( trace )
import qualified Data.Map as Map
import Data.List
import Nodes

data FSharp = FSharp

splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _   []  subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise            = split' sub (tail str) (head str:subacc) acc

importRuntime :: String
importRuntime = "module Lexer\n\nopen Token\nopen ParserCombinator"

compileExpr :: Expr -> String 
compileExpr (ParseStringNode str _) = "(literal_parse " ++ show str ++ ")"
compileExpr (ParseRuleNode id _) = "(fun _x -> " ++ id ++ " _x)"
compileExpr (ParseBinOpNode a op b _) = "(" ++ compileExpr a ++ " " ++ opf op ++ " " ++ compileExpr b ++ ")" where
    opf ">" = ">>>"
    opf "|" = "|||"
compileExpr (ParseUnaryNode op e _) = "(" ++ opf op ++ " " ++ compileExpr e ++ ")" where
    opf "*" = "!*"
    opf "+" = "!+"
    opf "?" = "!?"

compileProgramExpr :: String -> ProgramExpr -> String
compileProgramExpr _ (NumNode n _) = "(" ++ n ++ ")"
compileProgramExpr _ (IdentifierNode id _) = "(" ++ id ++ ")"
compileProgramExpr s (StrNode str _) = "(" ++ show str ++ ")"
compileProgramExpr s (CallNode str ids _) = "(" ++ s ++ "." ++ str ++ "(_t) " ++ unwords (map (compileProgramExpr s) ids) ++ ")"
compileProgramExpr _ (BoolNode b _) = showL b
compileProgramExpr s (IfNode c t e _) = "if " ++ compileProgramExpr s c ++ " then " ++ (
    case t of 
        Right a -> " ("++ compileProgramExpr s a ++ ") "
        Left a -> " (Ok(" ++ show a  ++ ")) "
    ) ++  " else " ++ maybe " (Ok \"\")" (\x -> " " ++ show x ++ " ") e
compileProgramExpr _ (RuleProgramNode r _) = "(" ++ compileExpr r ++ ")"
compileProgramExpr _ (EraseNode _) = show "_"
compileProgramExpr _ (ErrorNode str _) = "(Error(" ++ show str ++ "))"

compileRule :: RuleDef -> String 
compileRule (RuleDef id expr _) = "and " ++ id ++ " = " ++ compileExpr expr

compileFirstRule :: RuleDef -> String 
compileFirstRule (RuleDef id expr _) = "let rec " ++ id ++ " = " ++ compileExpr expr

compileExtractors :: [DataExtractor] -> String
compileExtractors ds = "let get_unmodified_toks tokClass (_x: Value) = tokens_from_rules tokClass [" ++ intercalate "; " (map (\(DataExtractor id exp _) -> "( " ++ show id ++ ", (" ++ compileExpr exp ++ ") )") ds) ++ "] _x"

compileModifiers :: String -> [DataModifier] -> String
compileModifiers supportCode mods = 
    "let get_toks tokClass (_x: string) = \n" ++
        " match get_unmodified_toks tokClass ([], _x) [] with\n" ++
        " | Ok(xs) -> modify_token_by_condition tokClass xs (Map([" ++ intercalate "; " (Map.foldrWithKey (\k v ks -> ("(" ++ show k ++ ", " ++ v ++ ")"):ks) [] $ Map.map (\xs -> "[" ++ intercalate "; " (map (\x -> "(fun _t -> " ++ compileProgramExpr supportCode x ++ ")") xs) ++ "]") (buildMap Map.empty mods)) ++ "]))\n" ++
        " | Error(err) -> Error(err)\n"

compileRules :: [RuleDef] -> [String]
compileRules rds = case rds of
    [] -> []
    x:xs -> compileFirstRule x : map compileRule xs

compileLexeme :: String -> Lexeme -> String
compileLexeme supportCode (Lexeme rds exts mods)  = importRuntime ++ "\n\n" ++ intercalate "\n" (compileRules rds) ++ "\n\n" ++ compileExtractors exts ++ "\n\n" ++ compileModifiers supportCode mods

instance CodeGenerator FSharp where
    generate _ = compileLexeme