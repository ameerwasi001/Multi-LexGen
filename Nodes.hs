module Nodes where

import Text.Megaparsec ( SourcePos )
import Data.List ( intercalate )
import Data.Char ( toLower )

showL :: Show a => a -> [Char]
showL k = toLower x : xs where (x:xs) = show k

data Expr = 
    ParseStringNode String SourcePos
    | ParseRuleNode String SourcePos
    | ParseBinOpNode Expr String Expr SourcePos
    | ParseUnaryNode String Expr SourcePos

data ProgramExpr =
    NumNode String SourcePos
    | StrNode String SourcePos
    | EraseNode SourcePos
    | ErrorNode String SourcePos
    | BoolNode Bool SourcePos
    | IdentifierNode String SourcePos
    | RuleProgramNode Expr SourcePos
    | IfNode ProgramExpr (Either String ProgramExpr) (Maybe String) SourcePos
    | CallNode String [ProgramExpr] SourcePos

data RuleDef = RuleDef String Expr SourcePos
data DataExtractor = DataExtractor String Expr SourcePos
data DataModifier = DataModifier String ProgramExpr SourcePos
data Lexeme = Lexeme [RuleDef] [DataExtractor] [DataModifier]

instance Show Expr where
    show (ParseStringNode str _) = show str
    show (ParseRuleNode id _) = id
    show (ParseBinOpNode a op b _) = "(" ++ show a ++ " " ++ op ++ " " ++ show b ++ ")"
    show (ParseUnaryNode op a _) = "(" ++ show a ++ ")" ++ op

instance Show ProgramExpr where
    show (StrNode str _) = show str
    show (EraseNode _) = "_"
    show (ErrorNode s _) = "!" ++ s
    show (NumNode n _) = n
    show (IdentifierNode id _) = id
    show (BoolNode t _) = showL t
    show (IfNode c t e _) = "if " ++ show c ++ " then " ++ show t ++ maybe "" (\x -> " else " ++ show x) e
    show (CallNode c as _) = c ++ "(" ++ intercalate ", " (map show as) ++ ")"

instance Show RuleDef where show (RuleDef id def _) = id ++ " = " ++ show def 
instance Show DataExtractor where show (DataExtractor id def _) = id ++ " = " ++ show def 
instance Show DataModifier where show (DataModifier id def _) = id ++ " ?= " ++ show def 
instance Show Lexeme where show (Lexeme rules extractors mods) = intercalate "\n" (map show rules) ++ "\n\n#Extractors\n" ++ intercalate "\n" (map show extractors) ++ "\n\n#Modifiers" ++ intercalate "\n" (map show mods)
