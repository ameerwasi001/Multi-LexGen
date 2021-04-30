module PythonGen where

import CodeGen ( CodeGenerator(..), compile, buildMap )
import Debug.Trace ( trace )
import Data.List ( intercalate )
import qualified Data.Map as Map
import Nodes

data Python = Python

importRuntime :: String
importRuntime = "from runtime.python.combinators import bin_op, unary_op, literal_parse, tokensFromRules, modifyTokenByCondition\nfrom runtime.python.errors import raiseInputError"

compileExpr :: Expr -> String 
compileExpr (ParseStringNode str _) = "lambda _x: literal_parse(" ++ show str ++ ", _x)"
compileExpr (ParseRuleNode id _) = "lambda _x: " ++ id ++ "(_x)"
compileExpr (ParseBinOpNode a op b _) = "lambda _x: bin_op(" ++ compileExpr a ++ ", \"" ++ op ++ "\", " ++ compileExpr b ++ ", _x)"
compileExpr (ParseUnaryNode op e _) = "lambda _x: unary_op(\"" ++ op ++ "\", " ++ compileExpr e ++ ", _x)"

compileProgramExpr :: String -> ProgramExpr -> String
compileProgramExpr _ (NumNode n _) = n
compileProgramExpr _ (IdentifierNode id _) = id
compileProgramExpr s (StrNode str _) = show str
compileProgramExpr s (CallNode str ids _) = s ++ "." ++ str ++ "(t, " ++ intercalate ", " (map (compileProgramExpr s) ids) ++ ")"
compileProgramExpr _ (BoolNode b _) = show b
compileProgramExpr s (IfNode c t e _) = 
    (
        case t of
            Right a -> compileProgramExpr s a
            Left a -> show a
        ) ++ " if " ++ compileProgramExpr s c ++ " else " ++ maybe "None" show e
compileProgramExpr _ (RuleProgramNode r _) = compileExpr r
compileProgramExpr _ (EraseNode _) = show "_"
compileProgramExpr _ (ErrorNode str _) = "raiseInputError(" ++ show str ++ ")"

compileRule :: RuleDef -> String 
compileRule (RuleDef id expr _) = id ++ " = " ++ compileExpr expr

compileExtractors :: [DataExtractor] -> String
compileExtractors ds = "getUnmodifiedToks = lambda tok_class, _x: tokensFromRules(tok_class, [" ++ intercalate ", " (map (\(DataExtractor id exp _) -> "(" ++ show id ++ ", " ++ compileExpr exp ++ ")") ds) ++ "], _x)"

compileModifiers :: String -> [DataModifier] -> String
compileModifiers supportCode mods = "getToks = lambda tok_class, _x: modifyTokenByCondition(tok_class, getUnmodifiedToks(tok_class, ([], _x)), {" ++ intercalate ", " (Map.foldrWithKey (\k v ks -> (show k ++ ": " ++ v):ks) [] $ Map.map (\xs -> "[" ++ intercalate ", " (map (\ x -> "lambda t: " ++ compileProgramExpr supportCode x) xs) ++ "]") (buildMap Map.empty mods)) ++ "})"

compileLexeme :: String -> Lexeme -> String
compileLexeme supportCode (Lexeme rds exts mods)  = importRuntime ++ "\nimport " ++ supportCode ++ "\n\n" ++ intercalate "\n" (map compileRule rds) ++ "\n\n" ++ compileExtractors exts ++ "\n\n" ++ compileModifiers supportCode mods

instance CodeGenerator Python where
    generate _ = compileLexeme
