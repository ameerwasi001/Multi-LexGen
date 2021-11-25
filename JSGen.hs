module JSGen where

import CodeGen ( CodeGenerator(..), compile, buildMap )
import Debug.Trace ( trace )
import qualified Data.Map as Map
import Data.List
import Nodes

data JS = JS

splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _   []  subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise            = split' sub (tail str) (head str:subacc) acc

importRuntime :: String
importRuntime = "import {Token} from \"./token.js\"\nimport {binOp, unaryOp, literalParse, tokensFromRules} from \"./combinators.ts\""

compileExpr :: Expr -> String 
compileExpr (ParseStringNode str _) = "_x=>literalParse(" ++ show str ++ ", _x)"
compileExpr (ParseRuleNode id _) = "_x=>" ++ id ++ "(_x)"
compileExpr (ParseBinOpNode a op b _) = "_x=>binOp(" ++ compileExpr a ++ ", \"" ++ op ++ "\", " ++ compileExpr b ++ ", _x)"
compileExpr (ParseUnaryNode op e _) = "_x=>unaryOp(\"" ++ op ++ "\", " ++ compileExpr e ++ ", _x)"

compileProgramExpr :: String -> ProgramExpr -> String
compileProgramExpr _ (NumNode n _) = n
compileProgramExpr _ (IdentifierNode id _) = id
compileProgramExpr s (StrNode str _) = show str
compileProgramExpr s (CallNode str ids _) = s ++ "." ++ str ++ "(_t, " ++ intercalate ", " (map (compileProgramExpr s) ids) ++ ")"
compileProgramExpr _ (BoolNode b _) = showL b
compileProgramExpr s (IfNode c t e _) = compileProgramExpr s c ++ (
    case t of 
        Right a -> " ? (" ++ compileProgramExpr s a ++ ")"
        Left a -> " ? " ++ show a  ++ ""
    ) ++  " : " ++ maybe "null" show e
compileProgramExpr _ (RuleProgramNode r _) = compileExpr r
compileProgramExpr _ (EraseNode _) = show "_"
compileProgramExpr _ (ErrorNode str _) = "raiseError(" ++ show str ++ ")"

compileRule :: RuleDef -> String 
compileRule (RuleDef id expr _) = "function " ++ id ++ "(_x) { return " ++ removeFirst4 (compileExpr expr) ++ " }"

removeFirst4 :: [a] -> [a]
removeFirst4 = tail. tail . tail . tail

compileExtractors :: [DataExtractor] -> String
compileExtractors ds = "const getUnmodifiedToks = (_x) => { return tokensFromRules([" ++ intercalate ", " (map (\(DataExtractor id exp _) -> "[" ++ show id ++ ", " ++ compileExpr exp ++ "]") ds) ++ "], _x) }"

-- compileModifiers :: String -> [DataModifier] -> String
-- compileModifiers supportCode mods = 
--     "const getToks = (_x) => { \n\t" ++
--         "match get_unmodified_toks((vec![], _x)) { \n\t\t" ++
--             "Ok(a) => modify_token_by_condition(a, make_map(vec![" ++ intercalate ", " (Map.foldrWithKey (\k v ks -> ("(" ++ show k ++ ", " ++ v ++ ")"):ks) [] $ Map.map (\xs -> "vec![" ++ intercalate ", " (map (\x -> "inline_fun!(_t: Token => ModifierRes := " ++ compileProgramExpr supportCode x ++ ") as ModifierType") xs) ++ "]") (buildMap Map.empty mods)) ++ "])),\n\t\t" ++
--             "Err(err) => return Err(err)\n\t" ++
--         "}\n" ++ 
--     "}"

compileLexeme :: String -> Lexeme -> String
compileLexeme supportCode (Lexeme rds exts mods)  = importRuntime ++ "\nimport * as lexerHelper from " ++ show supportCode ++ ";\n\n" ++ intercalate "\n" (map compileRule rds) ++ "\n\n" ++ compileExtractors exts

instance CodeGenerator JS where
    generate _ = compileLexeme