module Erb.Parser where

import Text.Parsec.String
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Language (emptyDef)
import Erb.Ruby
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P


def = emptyDef
    { P.commentStart   = "/*"
    , P.commentEnd     = "*/"
    , P.commentLine    = "#"
    , P.nestedComments = True
    , P.identStart     = letter
    , P.identLetter    = alphaNum <|> oneOf "_"
    , P.reservedNames  = ["if", "else", "case", "elsif"]
    , P.reservedOpNames= ["=>","=","+","-","/","*","+>","->","~>","!"]
    , P.caseSensitive  = True
    }

lexer       = P.makeTokenParser def
parens      = P.parens lexer
braces      = P.braces lexer
operator    = P.operator lexer
symbol      = P.symbol lexer
reservedOp  = P.reservedOp lexer
whiteSpace  = P.whiteSpace lexer
naturalOrFloat     = P.naturalOrFloat lexer
identifier  = P.identifier lexer

rubyexpression = buildExpressionParser table term <?> "expression"
        
table =     [ [ Infix ( reservedOp "+" >> return PlusOperation ) AssocLeft 
              , Infix ( reservedOp "-" >> return MinusOperation ) AssocLeft ]
            , [ Infix ( reservedOp "/" >> return DivOperation ) AssocLeft 
              , Infix ( reservedOp "*" >> return MultiplyOperation ) AssocLeft ]
            , [ Infix ( reservedOp "<<" >> return ShiftLeftOperation ) AssocLeft 
              , Infix ( reservedOp ">>" >> return ShiftRightOperation ) AssocLeft ]
            , [ Infix ( reservedOp "and" >> return AndOperation ) AssocLeft 
              , Infix ( reservedOp "or" >> return OrOperation ) AssocLeft ]
            , [ Infix ( reservedOp "==" >> return EqualOperation ) AssocLeft 
              , Infix ( reservedOp "!=" >> return DifferentOperation ) AssocLeft ]
            , [ Infix ( reservedOp ">" >> return AboveOperation ) AssocLeft 
              , Infix ( reservedOp ">=" >> return AboveEqualOperation ) AssocLeft
              , Infix ( reservedOp "<=" >> return UnderEqualOperation ) AssocLeft 
              , Infix ( reservedOp "<" >> return UnderOperation ) AssocLeft ]
            , [ Infix ( reservedOp "=~" >> return RegexpOperation ) AssocLeft 
              , Infix ( reservedOp "!~" >> return NotRegexpOperation ) AssocLeft ]
            , [ Prefix ( symbol "!" >> return NotOperation ) ]
            , [ Prefix ( symbol "-" >> return NegOperation ) ]
            , [ Infix ( reservedOp "?" >> return ConditionalValue ) AssocLeft ]
            , [ Infix ( reservedOp "." >> return MethodCall ) AssocLeft ]
            ]
term
    =   parens rubyexpression
    <|> objectterm
    <|> variablereference

blockinfo = many1 $ noneOf "}"

objectterm = do
    methodname <- identifier >>= return . Value . Literal
    isblock <- optionMaybe (symbol "{")
    case isblock of
        Just _ -> do
            b <- blockinfo
            symbol "}"
            return $ MethodCall methodname (BlockOperation b)
        Nothing -> return $ Object methodname

variablereference = identifier >>= return . Object . Value . Literal

rubystatement = rubyexpression >>= return . Puts

textblock :: Parser [RubyStatement]
textblock = do
    s <- many (noneOf "<")
    let returned = Puts $ Value $ Literal s
    isend <- optionMaybe eof
    case isend of
        Just _  -> return [returned]
        Nothing -> do
            char '<'
            isrub <- optionMaybe (char '%')
            let nextrun = case isrub of
                    Just _  -> rubyblock
                    Nothing -> textblock
            n <- nextrun
            return (returned : n)


rubyblock :: Parser [RubyStatement]
rubyblock = do
    isequal <- optionMaybe (char '=')
    parsed <- case isequal of
        Just _  -> spaces >> rubyexpression >>= return . Puts
        Nothing -> spaces >> rubystatement
    string "%>"
    n <- textblock
    return (parsed : n)

erbparser :: Parser [RubyStatement]
erbparser = textblock

parseErbFile fname = do
    input <- readFile fname
    return (runParser erbparser () fname input)
