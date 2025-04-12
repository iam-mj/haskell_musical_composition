module Input.Fundamental where

import Text.Parsec
import Input.State (MyParser)

-- tried using a lexer, but it doesn't agree with the IO monad

-- end of line
eol :: MyParser Char
eol = char '\n'

identifier :: MyParser String
identifier = many alphaNum

content :: MyParser Char
content = alphaNum <|> char '/' <|> char '(' <|> char ')' <|> space <|> char ',' <|> eol

-- what to parse between commas
commaContent :: MyParser Char
commaContent = alphaNum <|> char '/' <|> char '(' <|> char ')' <|> space <|> eol

braces :: MyParser a -> MyParser a
braces parser = do
    char '{'
    content <- parser
    char '}'
    return content

parens :: MyParser a -> MyParser a
parens parser = do
    char '('
    content <- parser
    char ')'
    return content

int :: MyParser Int
int = do
    minus <- optionMaybe $ char '-'
    num   <- many digit
    case minus of
        Nothing -> return $ read num
        Just _  -> return $ -(read num)

commaSep :: MyParser a -> MyParser [a]
commaSep parser = do
    first <- parser
    rest <- many (do
        char ','
        spaces
        parser)
    return (first : rest)