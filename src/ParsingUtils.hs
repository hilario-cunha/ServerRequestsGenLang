module ParsingUtils where

import Text.ParserCombinators.Parsec

ws :: Parser String
ws = many (oneOf " ")

atLeastOneWs :: Parser ()
atLeastOneWs = skipMany1 (oneOf " ")

-- A parser combinator which skips whitespaces from both sides
lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

sepByComma1 :: Parser a -> Parser [a]
sepByComma1 p = (sepBy1 (lexeme p) (char ','))

betweenBrackets :: Parser a -> Parser a
betweenBrackets = between (char '[') (char ']') 

betweenBracketsSepByComma :: Parser a -> Parser [a]
betweenBracketsSepByComma p = betweenBrackets (sepByComma1 p)

keywordP :: Parser a -> Parser a
keywordP p = ws *> p <* atLeastOneWs

keycharP :: Char -> Parser Char
keycharP k = keywordP $ char k

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseInt :: Parser Integer
parseInt = read <$> many1 digit
