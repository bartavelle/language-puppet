module Erb.Parser where

import           XPrelude.Extra         hiding (option, try)

import           Control.Exception      (catch)
import qualified Data.Text              as Text
import           Text.Parsec.Char
import           Text.Parsec.Combinator hiding (optional)
import           Text.Parsec.Error
import           Text.Parsec.Expr
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.Pos
import           Text.Parsec.Prim       hiding (many, (<|>))
import           Text.Parsec.String
import qualified Text.Parsec.Token      as P

import           Erb.Ruby

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
table =  [ [ Infix  ( reservedOp "+" >> return PlusOperation        ) AssocLeft
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
         -- , [ Infix  ( reservedOp "." >> return MethodCall           ) AssocLeft ]
         ]
term :: Parser Expression
term =
      parens rubyexpression
  <|> scopeLookup
  <|> stringLiteral
  <|> objectterm
  <|> variablereference

scopeLookup :: Parser Expression
scopeLookup = do
  void $ try $ string "scope"
  end <- (string ".lookupvar(" >> return (char ')')) <|> (char '[' >> return (char ']'))
  expr <- rubyexpression
  void end
  pure $ ScopeObject expr

stringLiteral :: Parser Expression
stringLiteral = Value `fmap` (doubleQuoted <|> singleQuoted)

doubleQuoted :: Parser Value
doubleQuoted = simplify <$> between (char '"') (char '"') quoteInternal
  where
    simplify [Value x] = x
    simplify x = Interpolable x
    quoteInternal = many (basicContent <|> interpvar <|> escaped)
    escaped = char '\\' >> (Value . Literal . Text.singleton) `fmap` anyChar
    basicContent = (Value . Literal . Text.pack) `fmap` many1 (noneOf "\"\\#")
    interpvar = do
      void $ try (string "#{")
      o <- many1 (noneOf "}")
      void $ char '}'
      return (Object (Value (Literal (Text.pack o))))

singleQuoted :: Parser Value
singleQuoted = Literal . Text.pack <$> between (char '\'') (char '\'') (many $ noneOf "'")

objectterm :: Parser Expression
objectterm = do
  arobase <- optional (char '@')
  methodname' <- toS <$> identifier
  let methodname = Value (Literal $ maybe methodname' (\a -> Text.cons a methodname') arobase)
  lookAhead anyChar >>= \case
    '[' -> do
        hr <- many (symbol "[" *> rubyexpression <* symbol "]")
        pure $! foldl LookupOperation (Object methodname) hr
    '{' -> fmap (MethodCall methodname . BlockOperation . Text.pack) (braces (many1 $ noneOf "}"))
    '(' -> fmap (MethodCall methodname . Value . Array) (parens (rubyexpression `sepBy` symbol ","))
    _ -> return $ Object methodname

variablereference :: Parser Expression
variablereference = fmap (Object . Value . Literal . Text.pack) identifier

rubystatement :: Parser RubyStatement
rubystatement = fail "statements not supported yet"

textblockW :: Maybe Char ->  Parser [RubyStatement]
textblockW c = do
  s <- many (noneOf "<")
  let ns = case c of
        Just x  -> x:s
        Nothing -> s
      returned = Puts $ Value $ Literal $ Text.pack ns
  optionMaybe eof >>= \case
    Just _  -> return [returned]
    Nothing -> do
      void $ char '<'
      n <- optionMaybe (char '%') >>= \case
        Just _  -> rubyblock
        Nothing -> textblockW (Just '<')
      pure (returned : n)

textblock :: Parser [RubyStatement]
textblock = textblockW Nothing

rubyblock :: Parser [RubyStatement]
rubyblock = do
  ps <- option [] (char '-' >> return [DropPrevSpace'])
  parsed <- optionMaybe (char '=') >>= \case
    Just _  -> spaces >> fmap (return . Puts) rubyexpression
    Nothing -> spaces >> many1 rubystatement
  spaces
  let dn (x:xs) = DropNextSpace x : xs
      dn x      = x
  ns <- option identity (char '-' >> return dn)
  void $ string "%>"
  n <- textblock
  pure (ps <> parsed <> ns n)

erbparser :: Parser [RubyStatement]
erbparser = textblock

parseErbFile :: FilePath -> IO (Either ParseError [RubyStatement])
parseErbFile fname =
  parseContent `catch` handler
  where
    parseContent = (runParser erbparser () fname . Text.unpack) `fmap` readFile fname
    handler e = let msg = show (e :: SomeException)
                in  return $ Left $ newErrorMessage (Message msg) (initialPos fname)

parseErbString :: String -> Either ParseError [RubyStatement]
parseErbString = runParser erbparser () mempty
