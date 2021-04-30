module Parser where

import Data.Void ( Void )
import Text.Megaparsec as P
import Text.Megaparsec.Char
import Debug.Trace ( trace )
import Data.List ( intercalate )
import qualified Text.Megaparsec.Char.Lexer as L
import Nodes

type Parser = Parsec Void String

eofParser :: Parser String
eofParser = "" <$ eof

lower :: Parser Char
lower = oneOf "abcdefghijklmnopqrstuvwxyz"
upper :: Parser Char
upper = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
digit :: Parser Char
digit = oneOf "1234567890"
space :: Parser Char
space = oneOf " " :: Parser Char
spaces :: Parser String
spaces = P.many $ oneOf " \t"
mspaces :: Parser String
mspaces = Parser.space *> Parser.spaces
newline :: Parser Char
newline = char '\n'
newlines :: Parser String
newlines = many Parser.newline

keyword :: Show a => a -> Parser String
keyword k = Text.Megaparsec.Char.string (showL k) :: Parser String

stringParser :: (String -> SourcePos -> a) -> Char -> Parser a
stringParser f c =
    do
        pos <- getSourcePos
        str <- (char c *> manyTill L.charLiteral (char c)) :: Parser String
        return $ f str pos

identifierParser :: (String -> SourcePos -> a) -> Parser Char -> Parser Char -> Parser a
identifierParser f fs sn =
    do
        pos <- getSourcePos
        fc <- fs
        lst <- P.many sn
        return $ f (fc:lst) pos

numParser :: Parser ProgramExpr
numParser =
    do
        pos <- getSourcePos
        fs <- digit :: Parser Char
        str <- P.many digit :: Parser String
        return $ NumNode (fs : str) pos

rule :: Parser Expr
rule = identifierParser ParseRuleNode lower (lower <|> upper <|> digit)

extractor :: Parser Expr
extractor = identifierParser ParseRuleNode upper (upper <|> digit)

binOp fa fb ops ret = do
  t1 <- fa
  loop t1
  where termSuffix t1 = try (do
          pos <- getSourcePos
          spaces
          op <- ops
          spaces
          t2 <- fb
          loop (ret t1 op t2 pos))
        loop t = termSuffix t <|> return t

parseLexeme :: Parser Lexeme
parseLexeme = do
        rules <- parseRuleDef `endBy` (eofParser <|> (spaces *> Parser.newline *> P.many Parser.newline <* spaces :: Parser String))
        extractors <- try parseExtractor `endBy` (eofParser <|> (spaces *> Parser.newline *> P.many Parser.newline <* spaces :: Parser String))
        modifiers <- parseModifier `endBy` (eofParser <|> (spaces *> Parser.newline *> P.many Parser.newline <* spaces :: Parser String))
        return $ Lexeme rules extractors modifiers

parseModifier :: Parser DataModifier
parseModifier = (\pos (ParseRuleNode id _) def -> DataModifier id def pos)
    <$> getSourcePos
    <*> extractor
    <*> (spaces *> Text.Megaparsec.Char.string "?=" *> spaces *> parseIf)

parseExtractor :: Parser DataExtractor
parseExtractor = (\pos (ParseRuleNode id _) def -> DataExtractor id def pos)
    <$> getSourcePos
    <*> (extractor <* spaces <* Text.Megaparsec.Char.string "=" <* spaces)
    <*> parseAlternativeRules

parseRuleDef :: Parser RuleDef
parseRuleDef = (\pos (ParseRuleNode id _) def -> RuleDef id def pos) 
    <$> getSourcePos 
    <*> (rule <* spaces <* Text.Megaparsec.Char.string "=" <* spaces) 
    <*> parseAlternativeRules

parseAlternativeRules :: Parser Expr
parseAlternativeRules = binOp parseRuleChain parseRuleChain (Text.Megaparsec.Char.string "|") ParseBinOpNode

parseChain :: (a -> String -> a -> SourcePos -> a) -> Parser a -> Parser a
parseChain binf lower = do 
    pos <- getSourcePos
    foldl1 (\ a b -> binf a ">" b pos) <$> parsePs
    where parsePs = P.many (lower <* spaces)

parseChainLs :: Parser a -> Parser [a]
parseChainLs lower = P.many (lower <* spaces)

oneBranchIfNode f pos t = IfNode (BoolNode Prelude.True pos) (f t) Nothing pos

parseIf :: Parser ProgramExpr
parseIf = try (
        (\pos c t -> IfNode c (Left t) Nothing pos)
        <$> getSourcePos
        <*> (spaces *> parseCall <* spaces <* Text.Megaparsec.Char.string "?")
        <*> (spaces *> ((\(ParseRuleNode id pos) -> id) <$> extractor) <* spaces)
    ) <|> try (
        oneBranchIfNode Left
        <$> getSourcePos 
        <*> ((\(ParseRuleNode id pos) -> id) <$> extractor)
    ) <|> (
        oneBranchIfNode Left <$> getSourcePos <*> Text.Megaparsec.Char.string "_"
    ) <|> (
        oneBranchIfNode Right <$> getSourcePos <*> programErrorParser
    )


parseRuleChain :: Parser Expr
parseRuleChain = parseChain ParseBinOpNode atomParser

parseCall :: Parser ProgramExpr
parseCall = try (CallNode 
    <$> ((\(IdentifierNode a _) -> a) <$> programIdentifierParser)
    <*> (spaces *> P.many (spaces *> programAtomParser <* spaces))
    <*> getSourcePos) <|> programAtomParser

postfixParser :: Parser Expr
postfixParser = choice $ map try [
        stringParser ParseStringNode '\'',
        rule,
        Text.Megaparsec.Char.string "(" *> spaces *> parseAlternativeRules <* spaces <* Text.Megaparsec.Char.string ")"
    ]

atomParser :: Parser Expr
atomParser = try ((\pos a op -> ParseUnaryNode op a pos) <$> getSourcePos <*> postfixParser <*> choice sign) <|> postfixParser where
    sign = [
        Text.Megaparsec.Char.string "+",
        Text.Megaparsec.Char.string "*",
        Text.Megaparsec.Char.string "?"
        ]

programIdentifierParser :: Parser ProgramExpr
programIdentifierParser = identifierParser IdentifierNode lower (lower <|> upper <|> digit)

programStringParser :: Parser ProgramExpr
programStringParser = stringParser StrNode '\''

programEraseParser :: Parser ProgramExpr
programEraseParser = EraseNode <$> (getSourcePos <* Text.Megaparsec.Char.string "_")

programErrorParser :: Parser ProgramExpr
programErrorParser = flip ErrorNode <$> getSourcePos <*> (Text.Megaparsec.Char.string "!" *> spaces *> stringParser const '\'')

programAtomParser :: Parser ProgramExpr
programAtomParser = choice $ map try [
        programStringParser,
        programIdentifierParser,
        numParser,
        Text.Megaparsec.Char.string "(" *> spaces *> parseIf <* spaces <* Text.Megaparsec.Char.string ")",
        flip RuleProgramNode <$> getSourcePos <*> (Text.Megaparsec.Char.string "{" *> spaces *> parseAlternativeRules <* spaces <* Text.Megaparsec.Char.string "}"),
        programEraseParser
    ]