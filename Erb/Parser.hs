module Erb.Parser where

import Text.Parsec.String
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Combinator
import Text.Parsec.Language (emptyDef)
import Erb.Ruby
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import qualified Data.Text as T
import Control.Monad.Identity

def :: P.GenLanguageDef String u Identity
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

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser def

parens :: Parser a -> Parser a
parens = P.parens lexer

braces :: Parser a -> Parser a
braces = P.braces lexer

operator :: Parser String
operator = P.operator lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = P.naturalOrFloat lexer

identifier :: Parser String
identifier = P.identifier lexer

rubyexpression :: Parser Expression
rubyexpression = buildExpressionParser table term <?> "expression"

table :: [[Operator String () Identity Expression]]
table =     [ [ Infix  ( reservedOp "+" >> return PlusOperation        ) AssocLeft
              , Infix  ( reservedOp "-" >> return MinusOperation       ) AssocLeft ]
            , [ Infix  ( reservedOp "/" >> return DivOperation         ) AssocLeft
              , Infix  ( reservedOp "*" >> return MultiplyOperation    ) AssocLeft ]
            , [ Infix  ( reservedOp "<<" >> return ShiftLeftOperation  ) AssocLeft
              , Infix  ( reservedOp ">>" >> return ShiftRightOperation ) AssocLeft ]
            , [ Infix  ( reservedOp "and" >> return AndOperation       ) AssocLeft
              , Infix  ( reservedOp "or" >> return OrOperation         ) AssocLeft ]
            , [ Infix  ( reservedOp "==" >> return EqualOperation      ) AssocLeft
              , Infix  ( reservedOp "!=" >> return DifferentOperation  ) AssocLeft ]
            , [ Infix  ( reservedOp ">" >> return AboveOperation       ) AssocLeft
              , Infix  ( reservedOp ">=" >> return AboveEqualOperation ) AssocLeft
              , Infix  ( reservedOp "<=" >> return UnderEqualOperation ) AssocLeft
              , Infix  ( reservedOp "<" >> return UnderOperation       ) AssocLeft ]
            , [ Infix  ( reservedOp "=~" >> return RegexpOperation     ) AssocLeft
              , Infix  ( reservedOp "!~" >> return NotRegexpOperation  ) AssocLeft ]
            , [ Prefix ( symbol "!" >> return NotOperation             )           ]
            , [ Prefix ( symbol "-" >> return NegOperation             )           ]
            , [ Infix  ( reservedOp "?" >> return ConditionalValue     ) AssocLeft ]
            , [ Infix  ( reservedOp "." >> return MethodCall           ) AssocLeft ]
            ]
term :: Parser Expression
term
    =   parens rubyexpression
    <|> scopeLookup
    <|> stringLiteral
    <|> objectterm
    <|> variablereference

scopeLookup :: Parser Expression
scopeLookup = do
    void $ try $ string "scope.lookupvar("
    expr <- rubyexpression
    void $ char ')'
    return $ Object expr

blockinfo :: Parser String
blockinfo = many1 $ noneOf "}"

stringLiteral :: Parser Expression
stringLiteral = doubleQuoted <|> singleQuoted

doubleQuoted :: Parser Expression
doubleQuoted = fmap (Value . Literal . T.pack) $ between (char '"') (char '"') (many $ noneOf "\"")

singleQuoted :: Parser Expression
singleQuoted = fmap (Value . Literal . T.pack) $ between (char '\'') (char '\'') (many $ noneOf "'")

objectterm :: Parser Expression
objectterm = do
    methodname <- fmap (Value . Literal . T.pack) identifier
    nc <- lookAhead anyChar
    case nc of
        '{' -> fmap (MethodCall methodname . BlockOperation . T.pack) (braces blockinfo)
        '(' -> fmap (MethodCall methodname . Value . Array) (parens (rubyexpression `sepBy` symbol ","))
        _ -> return $ Object methodname

variablereference :: Parser Expression
variablereference = fmap (Object . Value . Literal . T.pack) identifier

rubystatement :: Parser RubyStatement
rubystatement = fmap Puts rubyexpression

textblockW :: Maybe Char ->  Parser [RubyStatement]
textblockW c = do
    s <- many (noneOf "<")
    let ns = case c of
            Just x  -> x:s
            Nothing -> s
        returned = Puts $ Value $ Literal $ T.pack ns
    isend <- optionMaybe eof
    case isend of
        Just _  -> return [returned]
        Nothing -> do
            void $ char '<'
            isrub <- optionMaybe (char '%')
            n <- case isrub of
                Just _  -> rubyblock
                Nothing -> textblockW (Just '<')
            return (returned : n)

textblock :: Parser [RubyStatement]
textblock = textblockW Nothing

rubyblock :: Parser [RubyStatement]
rubyblock = do
    isequal <- optionMaybe (char '=')
    parsed <- case isequal of
        Just _  -> spaces >> fmap Puts rubyexpression
        Nothing -> spaces >> rubystatement
    spaces
    void $ try $ string "%>"
    n <- textblock
    return (parsed : n)

erbparser :: Parser [RubyStatement]
erbparser = textblock

parseErbFile :: FilePath -> IO (Either ParseError [RubyStatement])
parseErbFile fname = do
    input <- readFile fname
    return (runParser erbparser () fname input)
