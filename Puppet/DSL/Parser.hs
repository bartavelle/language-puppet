 module Puppet.DSL.Parser (
    parse,
    mparser
) where

{-
 unhandled :
    plusignment
    if without parens
    File['nagios_htpasswd', 'nagios_cgi_cfg'] { group => 'www-data' }    ( liste de overrides )
-}

import Data.Char
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Data.List.Utils
import Puppet.DSL.Types
import Debug.Trace

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
-- stringLiteral = P.stringLiteral lexer
naturalOrFloat     = P.naturalOrFloat lexer

lowerFirstChar :: String -> String
lowerFirstChar x = [toLower $ head x] ++ (tail x)

-- expression parser
exprparser = buildExpressionParser table term <?> "expression"
        
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
            ]
term = parens exprparser
    <|> puppetVariableOrHashLookup
    <|> puppetNumeric
    <|> puppetArray
    <|> puppetHash
    <|> try puppetResourceReference
    <|> try puppetFunctionCall
    <|> try puppetLiteralValue
    <|> puppetInterpolableString

hashRef = do { symbol "["
    ; e <- exprparser
    ; symbol "]"
    ; return e
    }

puppetVariableOrHashLookup = do { v <- puppetVariable
    ; whiteSpace
    ; hashlist <- many hashRef
    ; case hashlist of
        [] -> return $ Value (VariableReference v)
        x -> return $ makeLookupOperation v hashlist
    }

makeLookupOperation :: String -> [Expression] -> Expression
makeLookupOperation name exprs = foldl lookups (LookupOperation (Value (VariableReference name)) (head exprs)) (tail exprs)
    where
        lookups ctx v = LookupOperation ctx v

identstring = many1 (alphaNum <|> oneOf "-_")

identifier = do {
    x <- identstring
    ; whiteSpace
    ; return x
    }

puppetResourceReference = do { rtype <- puppetQualifiedReference
    ; symbol "["
    ; rname <- exprparser
    ; symbol "]"
    ; return $ Value (ResourceReference rtype rname)
    }

puppetResourceOverride = do { pos <- getPosition
    ; rtype <- puppetQualifiedReference
    ; symbol "["
    ; rname <- exprparser
    ; symbol "]"
    ; symbol "{"
    ; e <- puppetAssignment `sepEndBy` (symbol ",")
    ; symbol "}"
    ; return [ ResourceOverride rtype rname e pos ]
    }

puppetInclude = do { pos <- getPosition
    ; string "include"
    ; whiteSpace
    ; v <- puppetQualifiedName
    ; return [ Include v pos ]
    }

puppetRequire = do { pos <- getPosition
    ; string "require"
    ; whiteSpace
    ; v <- puppetLiteral
    ; return [ Require v pos ]
    }

puppetQualifiedName = do { firstletter <- lower
    ; parts <- identstring `sepEndBy` (string "::")
    ; whiteSpace
    ; return $ [firstletter] ++ (join "::" parts)
    }

puppetQualifiedReference = do { firstletter <- upper <?> "Uppercase letter for a reference"
    ; parts <- identstring `sepEndBy` (string "::")
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

puppetAssignment = do { n <- puppetLiteralValue
    ; symbol "=>"
    ; v <- exprparser
    ; return $ (n, v)
    }

nodeDeclaration = do { pos <- getPosition
    ; string "node"
    ; whiteSpace
    ; n <- puppetLiteral
    ; symbol "{"
    ; e <- many1 stmtparser
    ; symbol "}"
    ; return [ Node n (concat e) pos ]
    }

-- no trailing whiteSpace
puppetVariable = do { char '$'
    ; n <- do { char '{'
        ; o <- many1 (noneOf("}"))
        ; char '}'
        ; return o
        } 
    <|> do { o <- many1 ( try (string "::") <|> identstring )
        ; return $ concat o
        }
    ; return n
    }

puppetVariableReference = do { v <- puppetVariable
    ; whiteSpace
    ; return $ Value (VariableReference v)
    }

variableAssignment = do { pos <- getPosition
    ; varname <- puppetVariable
    ; whiteSpace
    ; symbol "="
    ; e <- exprparser
    ; return [VariableAssignment varname e pos]
    }

-- types de base
-- puppetLiteral : toutes les strings puppet

puppetLiteral = doubleQuotedString
    <|> singleQuotedString
    <|> identifier

puppetLiteralValue = do { v <- puppetLiteral
    ; return (Value (Literal v))
    }

puppetRegexp = do { symbol "/"
    ; v <- many (noneOf "/")
    ; symbol "/"
    ; return v
    }

singleQuotedString = do { char '\''
    ; v <- many ( do { char '\\' ; x <- anyChar; return [x] } <|> many1 (noneOf "'\\") )
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
        do { x <- doubleQuotedStringContent
            ; return $ Literal x
            }
        <|> try ( do { x <- puppetVariable
            ; return $ VariableReference x
            } )
        <|> do { char '$'
            ; return $ Literal "$"
            }
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

puppetNumeric = do { v <- naturalOrFloat
    ; return (case v of
            Left x -> (Value . Integer) x 
            Right x -> (Value . Double) x
        )
    }

puppetResourceGroup = do { virtcount <- many (char '@')
    ; v <- puppetQualifiedName
    ; symbol "{"
    ; x <- (resourceArrayDeclaration <|> resourceDeclaration) `sepEndBy` (symbol ";" <|> symbol ",")
    ; symbol "}"
    ; case virtcount of
        "" -> return $ map (\(rname, rvalues, pos) -> (Resource v rname rvalues Normal pos)) (concat x)
        "@" -> return $ map (\(rname, rvalues, pos) -> (Resource v rname rvalues Virtual pos)) (concat x)
        "@@" -> return $ map (\(rname, rvalues, pos) -> (Resource v rname rvalues Exported pos)) (concat x)
    }

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
        ; e <- puppetAssignment `sepEndBy` (symbol ",")
        ; symbol "}"
        ; return e
        })
    ; case chev of
        "<" -> return [ ResourceCollection rtype e overrides pos ]
        "<<" -> return [ VirtualResourceCollection rtype e overrides pos ]
        _ -> error $ "Invalid resource collection syntax at " ++ (show pos)
    }

resourceArrayDeclaration = do { pos <- getPosition
    ; v <- puppetArrayRaw
    ; symbol ":"
    ; x <- puppetAssignment `sepEndBy` symbol ","
    ; return $ map (\nm -> (nm, x, pos)) v
    }

resourceDeclaration = do { pos <- getPosition
    ; v <- (puppetVariableReference <|> try puppetLiteralValue <|> puppetInterpolableString )
    ; whiteSpace
    ; symbol ":"
    ; x <- puppetAssignment `sepEndBy` symbol ","
    ; return [(v, x, pos)]
    }

puppetResourceDefaults = do { pos <- getPosition
    ; rtype <- puppetQualifiedReference
    ; symbol "{"
    ; e <- puppetAssignment `sepBy` symbol ","
    ; symbol "}"
    ; return [ResourceDefault rtype e pos]
    }

puppetClassParameter = do { varname <- puppetVariable
    ; whiteSpace
    ; defaultvalue <- optionMaybe ( do { symbol "="
        ; e <- exprparser
        ; return e
        } )
    ; return (varname, defaultvalue)
    }

puppetClassParameters = do { symbol "("
    ; pmt <- puppetClassParameter `sepBy` symbol ","
    ; symbol ")"
    ; return pmt
    }

puppetClassDefinition = do { pos <- getPosition
    ; string "class"
    ; whiteSpace
    ; cname <- puppetQualifiedName
    ; params <- option [] puppetClassParameters
    ; cparent <- optionMaybe ( do { string "inherits"; whiteSpace ; p <- puppetQualifiedName; return p } )
    ; symbol "{"
    ; st <- many stmtparser
    ; symbol "}"
    ; return [ClassDeclaration cname cparent params (concat st) pos]
    }

puppetDefine = do { pos <- getPosition
    ; string "define"
    ; whiteSpace
    ; cname <- puppetQualifiedName
    ; params <- option [] puppetClassParameters
    ; symbol "{"
    ; st <- many stmtparser
    ; symbol "}"
    ; return [DefineDeclaration cname params (concat st) pos]
    }
    

puppetIfStyleCondition = do { pos <- getPosition
    ; symbol "("
    ; cond <- exprparser
    ; symbol ")"
    ; symbol "{"
    ; e <- many stmtparser
    ; symbol "}"
    ; return (cond, concat e)
    }
    
puppetElseIfCondition = do { pos <- getPosition
    ; try (string "elsif")
    ; whiteSpace
    ; out <- puppetIfStyleCondition
    ; return out
    }

puppetElseCondition = do { pos <- getPosition
    ; string "else"
    ; whiteSpace
    ; symbol "{"
    ; e <- many stmtparser
    ; symbol "}"
    ; return $ concat e
    }

puppetIfCondition = do { pos <- getPosition
    ; string "if"
    ; whiteSpace
    ; maincond <- puppetIfStyleCondition
    ; others <- option [] (many puppetElseIfCondition)
    ; elsec <- option [] puppetElseCondition
    ; return [ConditionalStatement ([maincond] ++ others ++ [(BTrue, elsec)]) pos]
    }

puppetCase = do { pos <- getPosition
    ; compares <- exprparser `sepBy` symbol ","
    ; symbol ":"
    ; symbol "{"
    ; st <- many stmtparser
    ; symbol "}"
    ; return ( compares, concat st )
    }

puppetRegexpCase = do { pos <- getPosition
    ; expression <- puppetRegexp
    ; symbol ":"
    ; symbol "{"
    ; st <- many stmtparser
    ; symbol "}"
    ; return ( [Value (PuppetRegexp expression)], concat st )
    }

defaultCase = do { pos <- getPosition
    ; string "default"
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
    ; string "case"
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
    ; symbol "("
    ; refs <- exprparser `sepBy` symbol ","
    ; symbol ")"
    ; return [MainFunctionCall name refs pos]
    }

puppetChains = do { pos <- getPosition
    ; refs <- puppetResourceReference `sepBy1` symbol "->"
    ; let refToPair (Value (ResourceReference rtype name)) = (rtype, name)
    ; let pairs = map refToPair refs
    ; let refpairs = zip pairs (tail pairs)
    ; return $ map (\((n1,v1),(n2,v2)) -> DependenceChain (n1,v1) (n2,v2) pos) refpairs
    }

puppetImport = do { pos <- getPosition
    ; string "import"
    ; whiteSpace
    ; pattern <- puppetLiteral
    ; return [Import pattern pos]
    }

stmtparser = variableAssignment
    <|> try (puppetInclude)
    <|> try (puppetRequire)
    <|> try (puppetResourceGroup)
    <|> try (puppetResourceDefaults)
    <|> try (puppetResourceOverride)
    <|> try (puppetResourceCollection)
    <|> try (puppetIfCondition)
    <|> try (puppetCaseCondition)
    <|> try (puppetMainFunctionCall)
    <|> try (puppetChains)
    <|> puppetImport
    <|> puppetClassDefinition
    <|> puppetDefine
    <|> nodeDeclaration

mparser = do {
        whiteSpace
        ; result <- many stmtparser
        ; eof
        ; return $ concat result
}
