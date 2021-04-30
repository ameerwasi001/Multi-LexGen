module CodeGen where

import Nodes ( Lexeme, ProgramExpr, DataModifier(DataModifier) )
import qualified Data.Map as Map
import Text.Megaparsec.Stream 
import Text.Megaparsec.Error
import Text.Megaparsec

buildMap :: Map.Map String [ProgramExpr] -> [DataModifier] -> Map.Map String [ProgramExpr]
buildMap mp [] = mp
buildMap mp ((DataModifier k e _):xs) = case k `Map.lookup` mp of
    Just a -> buildMap (Map.insert k (a ++ [e]) mp) xs
    Nothing -> buildMap (Map.singleton k [e] `Map.union` mp) xs


compile :: (CodeGenerator a, ShowErrorComponent e) => a -> String -> Parsec e String Lexeme -> String -> String -> String
compile mode supportCode parser fn content = case runParser parser fn content of
            Right n -> generate mode supportCode n
            Left e -> error $ errorBundlePretty e 

class CodeGenerator a where
    generate :: a -> String -> Lexeme -> String