{-|
This module exports the functions that will be useful to parse the DSL. They
should be able to parse everything you throw at them. The Puppet language is
extremely irregular, and most valid constructs are not documented in the
official language guide. This parser has been created by parsing the author's
own large manifests and the public Wikimedia ones.

Things that are known to not to be properly supported are :

    *  \"plussignement\" such as foo +\> bar. How to handle this is far from
    being obvious, as its actual behaviour is not documented.
-}
module Puppet.DSL.Parser (
    parse,
    mparser,
    exprparser
) where

import Data.Char
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Data.List.Utils
import Puppet.DSL.Types
import qualified Data.Map as Map
import Puppet.NativeTypes
import Control.Monad (when)

def = emptyDef
    { P.commentStart   = "/*"
    , P.commentEnd     = "*/"
    , P.commentLine    = "#"
    , P.nestedComments = True
    , P.identStart     = letter
    , P.identLetter    = alphaNum <|> char '_'
    , P.reservedNames  = ["if", "else", "case", "elsif", "default", "import", "define", "class", "node", "inherits", "true", "false", "undef"]
    , P.reservedOpNames= ["=>","=","+","-","/","*","+>","->","~>","!"]
    , P.caseSensitive  = True
    }

lexer       = P.makeTokenParser def
parens      = P.parens lexer
--braces      = P.braces lexer
--operator    = P.operator lexer
symbol      = P.symbol lexer
reservedOp  = P.reservedOp lexer
reserved    = P.reserved lexer
whiteSpace  = P.whiteSpace lexer
-- stringLiteral = P.stringLiteral lexer
naturalOrFloat     = P.naturalOrFloat lexer

lowerFirstChar :: String -> String
lowerFirstChar x = toLower (head x) : tail x

-- expression parser
{-| This is a parser for Puppet 'Expression's. -}
exprparser = buildExpressionParser table term <?> "expression"

table =     [
              [ Infix ( reservedOp "?" >> return ConditionalValue ) AssocLeft ]
            , [ Prefix ( symbol "-" >> return NegOperation ) ]
            , [ Prefix ( symbol "!" >> return NotOperation ) ]
            , [ Infix ( reserved   "in" >> return IsElementOperation ) AssocLeft ]
            , [ Infix ( reservedOp "/" >> return DivOperation ) AssocLeft
              , Infix ( reservedOp "*" >> return MultiplyOperation ) AssocLeft ]
            , [ Infix ( reservedOp "+" >> return PlusOperation ) AssocLeft
              , Infix ( reservedOp "-" >> return MinusOperation ) AssocLeft ]
            , [ Infix ( reservedOp "<<" >> return ShiftLeftOperation ) AssocLeft
              , Infix ( reservedOp ">>" >> return ShiftRightOperation ) AssocLeft ]
            , [ Infix ( reservedOp "==" >> return EqualOperation ) AssocLeft
              , Infix ( reservedOp "!=" >> return DifferentOperation ) AssocLeft ]
            , [ Infix ( reservedOp ">" >> return AboveOperation ) AssocLeft
              , Infix ( reservedOp ">=" >> return AboveEqualOperation ) AssocLeft
              , Infix ( reservedOp "<=" >> return UnderEqualOperation ) AssocLeft
              , Infix ( reservedOp "<" >> return UnderOperation ) AssocLeft ]
            , [ Infix ( reserved   "and" >> return AndOperation ) AssocLeft
              , Infix ( reserved   "or" >> return OrOperation ) AssocLeft ]
            , [ Infix ( reservedOp "=~" >> return RegexpOperation ) AssocLeft
              , Infix ( reservedOp "!~" >> return NotRegexpOperation ) AssocLeft ]
            ]
term = parens exprparser
    <|> puppetInterpolableString
    <|> puppetUndefined
    <|> puppetRegexpExpr
    <|> puppetVariableOrHashLookup
    <|> puppetNumeric
    <|> puppetArray
    <|> puppetHash
    <|> try puppetResourceReference
    <|> try puppetFunctionCall
    <|> puppetLiteralValue
    <?> "Expression terminal"

hashRef = do { symbol "["
    ; e <- exprparser
    ; symbol "]"
    ; return e
    }

puppetVariableOrHashLookup = do
    v <- puppetVariable
    whiteSpace
    hashlist <- many hashRef
    when (v == "string") $ unexpected "You are not allowed to name variables $string."
    case hashlist of
        [] -> return $ Value (VariableReference v)
        _ -> return $ makeLookupOperation v hashlist

makeLookupOperation :: String -> [Expression] -> Expression
makeLookupOperation name exprs = foldl LookupOperation (LookupOperation (Value (VariableReference name)) (head exprs)) (tail exprs)

identstring = many1 (alphaNum <|> char '_')

identifier = do {
    x <- identstring
    ; whiteSpace
    ; return x
    }

puppetResourceReference = do { rtype <- puppetQualifiedReference
    ; symbol "["
    ; rnames <- exprparser `sepBy` symbol ","
    ; symbol "]"
    ; if length rnames == 1
        then return $ Value (ResourceReference rtype (head rnames))
        else return $ Value $ PuppetArray $ map (Value . ResourceReference rtype) rnames
    }

puppetResourceOverride = do { pos <- getPosition
    ; rtype <- puppetQualifiedReference
    ; symbol "["
    ; rname <- exprparser `sepBy` symbol ","
    ; symbol "]"
    ; symbol "{"
    ; e <- puppetAssignment `sepEndBy` symbol ","
    ; symbol "}"
    ; return (map (\n -> ResourceOverride rtype n e pos) rname)
    }

puppetInclude = do { pos <- getPosition
    ; try $ reserved "include"
    ; vs <- (puppetQualifiedName <|> puppetLiteral) `sepBy` (symbol ",")
    ; return $ map (\v -> Include v pos) vs
    }

puppetRequire = do { pos <- getPosition
    ; try $ reserved "require"
    ; v <- puppetLiteral `sepBy` (symbol ",")
    ; return $ map (\x -> Require x pos) v
    }

puppetQualifiedName = do { optional (string "::")
    ; firstletter <- lower
    ; parts <- identstring `sepBy` (try $ string "::")
    ; whiteSpace
    ; return $ [firstletter] ++ (join "::" parts)
    }

puppetQualifiedReference = do { optional (string "::")
    ; firstletter <- upper <?> "Uppercase letter for a reference"
    ; parts <- identstring `sepBy` (string "::")
    ; whiteSpace
    ; return $ [toLower firstletter] ++ (join "::" $ map lowerFirstChar parts)
    }

puppetFunctionCall = do { funcname <- identifier
    ; symbol "("
    ; e <- exprparser `sepEndBy` (symbol ",")
    ; symbol ")"
    ; return $ Value (FunctionCall funcname e)
    }

puppetArrayRaw =  do { symbol "["
    ; e <- exprparser `sepEndBy` (symbol ",")
    ; symbol "]"
    ; return e
    }

puppetArray = do { e <- puppetArrayRaw
    ; return $ Value (PuppetArray e)
    }

puppetHash = do { symbol "{"
    ; e <- puppetAssignment `sepEndBy` (symbol ",")
    ; symbol "}"
    ; return $ Value (PuppetHash (Parameters e))
    }

puppetAssignment = do { n <- exprparser
    ; symbol "=>"
    ; v <- exprparser
    ; return $ (n, v)
    }

nodeDeclaration = do { pos <- getPosition
    ; try $ reserved "node"
    ; whiteSpace
    ; n <- puppetRegexp <|> puppetLiteral -- TODO HANDLE
    ; symbol "{"
    ; e <- many stmtparser
    ; symbol "}"
    ; return [ Node n (concat e) pos ]
    }

-- no trailing whiteSpace
puppetVariable = do
    char '$'
    choice
        [ do { char '{' ; o <- many1 $ noneOf "}" ; char '}' ; return o }
        , do { s <- option "" (string "::") ; o <- identstring `sepBy` (try $ string "::") ; return $ s ++ (join "::" o) }
        ]

variableAssignment = do
    pos <- getPosition
    varname <- puppetVariable
    whiteSpace
    symbol "="
    e <- exprparser
    when (varname == "string") $ unexpected "You are not allowed to name variables $string."
    return [VariableAssignment varname e pos]

-- types de base
-- puppetLiteral : toutes les strings puppet

puppetLiteral = doubleQuotedString
    <|> singleQuotedString
    <|> puppetQualifiedName
    <|> identifier

puppetLiteralValue = do { v <- puppetLiteral
    ; return (Value (Literal v))
    }

puppetRegexp = do { char '/'
    ; v <- many ( do { char '\\' ; x <- anyChar; return ['\\', x] } <|> many1 (noneOf "/\\") )
    ; symbol "/"
    ; return $ concat v
    }

puppetRegexpExpr = puppetRegexp >>= return . Value . PuppetRegexp

singleQuotedString = do { char '\''
    ; v <- many ( do { char '\\' ; x <- anyChar; if x=='\'' then return "'" else return ['\\',x] } <|> many1 (noneOf "'\\") )
    ; char '\''
    ; whiteSpace
    ; return $ concat v
    }

doubleQuotedString = do { char '"'
    ; v <- option "" doubleQuotedStringContent
    ; char '"'
    ; whiteSpace
    ; return v
    }

puppetInterpolableString = do { char '"'
    ; v <- many (
        try ( do { x <- puppetVariable
            ; when (x == "string") $ unexpected "You are not allowed to name variables $string."
            ; return $ VariableReference x
            } )
        <|> do { x <- doubleQuotedStringContent
            ; return $ Literal x
            }
        <|> do { char '$'
            ; return $ Literal "$"
            }
        <?> "Interpolable string content"
        )
    ; char '"'
    ; whiteSpace
    ; return $ Value (Interpolable v)
    }

doubleQuotedStringContent = do { x <- many1 (do { char '\\' ; x <- anyChar; return [stringEscape x] } <|> many1 (noneOf "\"\\$") )
    ; return $ concat x
    }

stringEscape 'n' = '\n'
stringEscape 't' = '\t'
stringEscape 'r' = '\r'
stringEscape '"' = '"'
stringEscape '\\' = '\\'
stringEscape '$' = '$'
stringEscape x = error $ "unknown escape pattern \\" ++ [x]

puppetUndefined = do
    try $ string "undef"
    whiteSpace
    return $ Value $ Undefined

puppetNumeric = do { v <- naturalOrFloat
    ; return (case v of
            Left x -> (Value . Integer) x
            Right x -> (Value . Double) x
        )
    }

puppetResourceGroup = do
    (virtcount, v) <- try ( do {
        virtcount <- many (char '@')
        ; v <- puppetQualifiedName
        ; symbol "{"
        ; return (virtcount, v)
    } )
    x <- (resourceArrayDeclaration <|> resourceDeclaration) `sepEndBy` (symbol ";" <|> symbol ",")
    symbol "}"
    case virtcount of
        ""      -> return $ map (\(rname, rvalues, pos) -> (Resource v rname rvalues Normal pos)) (concat x)
        "@"     -> return $ map (\(rname, rvalues, pos) -> (Resource v rname rvalues Virtual pos)) (concat x)
        "@@"    -> return $ map (\(rname, rvalues, pos) -> (Resource v rname rvalues Exported pos)) (concat x)
        _       -> unexpected "Too many @'s"

-- todo parse resource collection properly
puppetResourceCollection = do { pos <- getPosition
    ; rtype <- puppetQualifiedReference
    ; chev <- many1 (char '<')
    ; symbol "|"
    ; e <- option BTrue exprparser
    ; symbol "|"
    ; many1 (char '>')
    ; whiteSpace
    ; overrides <- option [] (do { symbol "{"
        ; ne <- puppetAssignment `sepEndBy` (symbol ",")
        ; symbol "}"
        ; return ne
        })
    ; case chev of
        "<" -> return [ VirtualResourceCollection rtype e overrides pos ]
        "<<" -> return [ ResourceCollection rtype e overrides pos ]
        _ -> error $ "Invalid resource collection syntax at " ++ (show pos)
    }

resourceArrayDeclaration = do { pos <- getPosition
    ; v <- puppetArrayRaw
    ; symbol ":"
    ; x <- puppetAssignment `sepEndBy` symbol ","
    ; return $ map (\nm -> (nm, x, pos)) v
    }

resourceDeclaration = do { pos <- getPosition
    ; v <- (puppetVariableOrHashLookup <|> puppetInterpolableString <|> puppetLiteralValue )
    ; whiteSpace
    ; symbol ":"
    ; x <- puppetAssignment `sepEndBy` symbol ","
    ; return [(v, x, pos)]
    }

puppetResourceDefaults = do { pos <- getPosition
    ; rtype <- puppetQualifiedReference
    ; symbol "{"
    ; e <- puppetAssignment `sepEndBy` symbol ","
    ; symbol "}"
    ; return [ResourceDefault rtype e pos]
    }

puppetClassParameter = do { varname <- puppetVariable
    ; whiteSpace
    ; defaultvalue <- optionMaybe ( do { symbol "="
        ; e <- exprparser
        ; return e
        } )
    ; when (varname == "string") $ unexpected "You are not allowed to name variables $string."
    ; return (varname, defaultvalue)
    }

puppetClassParameters = do { symbol "("
    ; pmt <- puppetClassParameter `sepBy` symbol ","
    ; symbol ")"
    ; return pmt
    }

puppetClassDefinition = do { pos <- getPosition
    ; try $ reserved "class"
    ; cname <- puppetQualifiedName
    ; params <- option [] puppetClassParameters
    ; cparent <- optionMaybe ( do { string "inherits"; whiteSpace ; p <- puppetQualifiedName; return p } )
    ; symbol "{"
    ; st <- many stmtparser
    ; symbol "}"
    ; return [ClassDeclaration cname cparent params (concat st) pos]
    }

puppetDefine = do
    pos <- getPosition
    try $ reserved "define"
    cname <- puppetQualifiedName
    params <- option [] puppetClassParameters
    symbol "{"
    st <- many stmtparser
    symbol "}"
    case Map.lookup cname baseNativeTypes of
        Just _  -> unexpected "Can't use a native type name for a define."
        Nothing -> return [DefineDeclaration cname params (concat st) pos]

puppetIfStyleCondition = do { cond <- exprparser <?> "Conditional expression"
    ; symbol "{"
    ; e <- many stmtparser
    ; symbol "}"
    ; return (cond, concat e)
    }

puppetElseIfCondition = do { reservedOp "elsif"
    ; whiteSpace
    ; out <- puppetIfStyleCondition
    ; return out
    }

puppetElseCondition = do { reservedOp "else"
    ; whiteSpace
    ; symbol "{"
    ; e <- many stmtparser
    ; symbol "}"
    ; return $ concat e
    }

puppetIfCondition = do { pos <- getPosition
    ; reserved "if"
    ; whiteSpace
    ; maincond <- puppetIfStyleCondition
    ; others <- option [] (many puppetElseIfCondition)
    ; elsec <- option [] puppetElseCondition
    ; return [ConditionalStatement ([maincond] ++ others ++ [(BTrue, elsec)]) pos]
    }

puppetCase = do {
      compares <- exprparser `sepBy` symbol ","
    ; symbol ":"
    ; symbol "{"
    ; st <- many stmtparser
    ; symbol "}"
    ; return ( compares, concat st )
    }

puppetRegexpCase = do {
      expression <- puppetRegexp
    ; symbol ":"
    ; symbol "{"
    ; st <- many stmtparser
    ; symbol "}"
    ; return ( [Value (PuppetRegexp expression)], concat st )
    }

defaultCase = do {
      string "default"
    ; symbol ":"
    ; symbol "{"
    ; st <- many stmtparser
    ; symbol "}"
    ; return ( [BTrue], concat st )
    }

condsToExpression :: Expression -> ([Expression], [Statement]) -> [(Expression, [Statement])]
condsToExpression e (exprs, stmts) = map (\x -> condToExpression e (x, stmts)) exprs

condToExpression :: Expression -> (Expression, [Statement]) -> (Expression, [Statement])
condToExpression _ (BTrue, stmts) = (BTrue, stmts)
condToExpression e (Value (PuppetRegexp regexp), stmts) = (RegexpOperation e (Value (PuppetRegexp regexp)), stmts)
condToExpression e (cnd, stmts) = (EqualOperation e cnd, stmts)

puppetCaseCondition = do { pos <- getPosition
    ; reservedOp "case"
    ; whiteSpace
    ; expr1 <- exprparser
    ; symbol "{"
    ; condlist <- many1 (puppetRegexpCase <|> try defaultCase <|> puppetCase)
    ; symbol "}"
    ; return $ [ConditionalStatement (concat (map (\x -> condsToExpression expr1 x) condlist)) pos]
    }

puppetMainFunctionCall = do { pos <- getPosition
    ; name <- identifier
    ; whiteSpace
    ; hasParens <- optionMaybe $ symbol "("
    ; refs <- exprparser `sepEndBy` symbol ","
    ; case hasParens of
        Just _ -> symbol ")"
        _      -> return ""
    ; return [MainFunctionCall name refs pos]
    }

puppetChains = do { pos <- getPosition
    ; refs <- try (puppetResourceReference `sepBy1` symbol "->")
    ; let refToPair (Value (ResourceReference rtype name)) = (rtype, name)
          refToPair x = error $ "Could not run refToPair on " ++ show x
    ; let pairs = map refToPair refs
    ; let refpairs = zip pairs (tail pairs)
    ; return $ map (\((n1,v1),(n2,v2)) -> DependenceChain (n1,v1) (n2,v2) pos) refpairs
    }

puppetImport = do { pos <- getPosition
    ; try $ reserved "import"
    ; pattern <- puppetLiteral
    ; return [Import pattern pos]
    }

stmtparser = variableAssignment
    <|> puppetInclude
    <|> puppetRequire
    <|> puppetImport
    <|> nodeDeclaration
    <|> puppetDefine
    <|> puppetIfCondition
    <|> puppetCaseCondition
    <|> puppetResourceGroup
    <|> try (puppetResourceDefaults)
    <|> try (puppetResourceOverride)
    <|> try (puppetResourceCollection)
    <|> puppetClassDefinition
    <|> puppetChains
    <|> puppetMainFunctionCall
    <?> "Statement"

mparser = do {
        whiteSpace
        ; result <- many stmtparser
        ; eof
        ; return $ concat result
}

