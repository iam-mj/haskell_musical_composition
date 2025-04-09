module Input.Lexer where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token as Token
import Text.Parsec.Language

languageDef = emptyDef { 
        identStart = letter,
        identLetter = alphaNum,
        reservedNames = ["music", "show", "context", "play", "save", "note:", "rest:", "duo:", 
                         "chord:", "modify", "insert", "delete", "replace", "transpose"],
        reservedOpNames = ["||", "++"]
    }

lexer = makeTokenParser languageDef

-- parsers - they get rid of the whitespace after the tokens
myIdentifier = Token.identifier lexer -- parses an identifier
myReserved   = Token.reserved   lexer -- parses a reserved name
myReservedOp = Token.reservedOp lexer -- parses an operator
myBraces     = Token.braces     lexer -- braces p parses p enclosed in braces ('{' and '}'), returning the value of p
myParens     = Token.parens     lexer -- parses p enclosed in parentases
myInteger    = Token.integer    lexer -- parses an integer
myWhiteSpace = Token.whiteSpace lexer -- parses whitespace
myCommaSep1  = Token.commaSep1 lexer  -- parses p separeted by commas