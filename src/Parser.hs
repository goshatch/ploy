{-# LANGUAGE OverloadedStrings #-}

-- | Main parser for Ploy!
module Parser (parseSingle, parseMultiple) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import SchemeTypes
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- | The type of Ploy's parser
-- Void means we don't have custom error components
-- Text means we're parsing Text input
type Parser = Parsec Void Text

-- | Parse spaces and comments
-- This handles whitespace between tokens
spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment ";")
    (L.skipBlockComment "#|" "|#")

-- | Parse a lexeme -- a token possibly followed by whiteSpace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Parse a specific symbol
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- | Parse something between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse an integer number
-- The `signed` combinator handles optional +/- prefix
parseNumber :: Parser LispVal
parseNumber = do
  n <- lexeme $ L.signed spaceConsumer L.decimal
  return (Number n)

-- | Parse a string literal
parseString :: Parser LispVal
parseString = lexeme $ do
  _ <- char '"' -- Opening quote
  contents <- manyTill L.charLiteral (char '"') -- Everything until closing quote
  return $ String (T.pack contents)

-- | Parse an atom (symbol)
-- Must start with a letter or symbol char, then can have more chars
parseAtom :: Parser LispVal
parseAtom = lexeme $ do
  first <- letterChar <|> symChar
  rest <- many (alphaNumChar <|> symChar)
  let atom = T.pack (first : rest)
  return $ case atom of
    "#t" -> Bool True -- Special case for boolean literals
    "#f" -> Bool False
    _ -> Atom atom
  where
    symChar = oneOf ("!#$%&|*+-/:<=>?@^_~" :: String)

-- | Parse a list expression
parseList :: Parser LispVal
parseList = parens $ do
  elements <- many parseExpr -- Parse zero or more expressions
  return $ case elements of
    [] -> Nil -- Empty list is Nil
    xs -> List xs -- Non-empty becomes list

-- | Parse a quoted expression
-- 'x is syntactic sugar for (quote x)
parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- TODO: unquoted expressions, quasiquoted expressions

-- | Main expression parser
-- Tries each type of expression in turn
parseExpr :: Parser LispVal
parseExpr =
  choice
    [ parseNumber,
      parseString,
      parseQuoted,
      parseAtom,
      parseList
    ]

-- | Parse multiple expressions
parseProgram :: Parser [LispVal]
parseProgram = do
  spaceConsumer -- Skip leading whitespace
  exprs <- many parseExpr -- parse many expressions
  eof -- must consume all input
  return exprs

-- | Main entry point: parse a single expression
parseSingle :: Text -> Either (ParseErrorBundle Text Void) LispVal
parseSingle = parse (spaceConsumer *> parseExpr <* eof) "<input>"

-- | Parse multiple expressions (use this for source files)
parseMultiple :: Text -> Either (ParseErrorBundle Text Void) [LispVal]
parseMultiple = parse parseProgram "<input>"

-- | For use in the REPL later
-- parserHelper :: Parser a -> Text -> Either Text a
-- parserHelper p input =
--   case parse p "<input>" input of
--     Left err -> Left $ T.pack $ errorBundlePretty err
--     Right val -> Right val
