{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Puppet.Parser (
    expression
  , puppetParser
  , runPParser
) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.HashSet as HS
import qualified Data.Maybe.Strict as S
import qualified Data.Foldable as F
import Data.Tuple.Strict hiding (fst,zip)
import Text.Regex.PCRE.ByteString.Utils

import Data.Char
import Control.Monad
import Control.Applicative
import Control.Lens hiding (noneOf)

import Puppet.Parser.Types
import Puppet.Utils

import Text.Parsec.Expr
import Text.Parser.Token hiding (stringLiteral')
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parsec.Pos (SourcePos,SourceName)
import Text.Parser.LookAhead
import Text.Parser.Token.Highlight
import Text.Parsec.Error (ParseError)
import qualified Text.Parsec.Prim as PP
import Text.Parsec.Text ()
import Data.Scientific

newtype Parser a = ParserT { unParser :: PP.ParsecT T.Text () Identity a}
                 deriving (Functor, Applicative, Alternative)

deriving instance Monad Parser
deriving instance Parsing Parser
deriving instance CharParsing Parser
deriving instance LookAheadParsing Parser

getPosition :: Parser SourcePos
getPosition = ParserT PP.getPosition

runPParser :: Parser a -> SourceName -> T.Text -> Either ParseError a
runPParser (ParserT p) = PP.parse p

type OP = PP.ParsecT T.Text () Identity

instance TokenParsing Parser where
    someSpace = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment)
      where
        simpleSpace = skipSome (satisfy isSpace)
        oneLineComment = char '#' >> void (manyTill anyChar newline)
        multiLineComment = try (string "/*") >> inComment
        inComment =     void (try (string "*/"))
                    <|> (skipSome (noneOf "*/") >> inComment)
                    <|> (oneOf "*/" >> inComment)

variable :: Parser Expression
variable = PValue . UVariableReference <$> variableReference

stringLiteral' :: Parser T.Text
stringLiteral' = char '\'' *> interior <* symbolic '\''
    where
        interior = T.pack . concat <$> many (some (noneOf "'\\") <|> (char '\\' *> fmap escape anyChar))
        escape '\'' = "'"
        escape x = ['\\',x]

identifierStyle :: IdentifierStyle Parser
identifierStyle = IdentifierStyle "Identifier" (satisfy acceptable) (satisfy acceptable) HS.empty Identifier ReservedIdentifier
    where
        acceptable x = isAsciiLower x || isAsciiUpper x || isDigit x || (x == '_')

identl :: Parser Char -> Parser Char -> Parser T.Text
identl fstl nxtl = do
        f   <- fstl
        nxt <- token $ many nxtl
        return $ T.pack $ f : nxt

operator :: String -> Parser ()
operator = void . highlight Operator . try . symbol

reserved :: String -> Parser ()
reserved = reserve identifierStyle

variableName :: Parser T.Text
variableName = do
    let acceptablePart = T.pack <$> ident identifierStyle
    out <- qualif acceptablePart
    when (out == "string") (fail "The special variable $string should never be used")
    return out

qualif :: Parser T.Text -> Parser T.Text
qualif p = token $ do
    header <- T.pack <$> option "" (try (string "::"))
    ( header <> ) . T.intercalate "::" <$> p `sepBy1` try (string "::")

qualif1 :: Parser T.Text -> Parser T.Text
qualif1 p = try $ do
    r <- qualif p
    unless ("::" `T.isInfixOf` r) (fail "This parser is not qualified")
    return r

className :: Parser T.Text
className = qualif moduleName

-- yay with reserved words
typeName :: Parser T.Text
typeName = className

moduleName :: Parser T.Text
moduleName = genericModuleName False

resourceNameRef :: Parser T.Text
resourceNameRef = qualif (genericModuleName True)

genericModuleName :: Bool -> Parser T.Text
genericModuleName isReference = do
    let acceptable x = isAsciiLower x || isDigit x || (x == '_')
        firstletter = if isReference
                          then fmap toLower (satisfy isAsciiUpper)
                          else satisfy isAsciiLower
    identl firstletter (satisfy acceptable)

parameterName :: Parser T.Text
parameterName = moduleName

-- this is not a token !
inBraces :: Parser T.Text
inBraces =  between (char '{') (char '}') (T.pack <$> some (satisfy (/= '}')))

variableReference :: Parser T.Text
variableReference = do
    void (char '$')
    v <- lookAhead anyChar >>= \case
         '{' -> inBraces
         _   -> variableName
    when (v == "string") (fail "The special variable $string must not be used")
    return v

interpolableString :: Parser (V.Vector UValue)
interpolableString = V.fromList <$> between (char '"') (symbolic '"')
     ( many (fmap UVariableReference interpolableVariableReference <|> doubleQuotedStringContent <|> fmap (UString . T.singleton) (char '$')) )
    where
        doubleQuotedStringContent = UString . T.pack . concat <$>
            some ((char '\\' *> fmap stringEscape anyChar) <|> some (noneOf "\"\\$"))
        stringEscape :: Char -> String
        stringEscape 'n'  = "\n"
        stringEscape 't'  = "\t"
        stringEscape 'r'  = "\r"
        stringEscape '"'  = "\""
        stringEscape '\\' = "\\"
        stringEscape '$'  = "$"
        stringEscape x    = ['\\',x]
        -- this is specialized because we can't be "tokenized" here
        variableAccept x = isAsciiLower x || isAsciiUpper x || isDigit x || x == '_'
        interpolableVariableReference = try $ do
            void (char '$')
            v <- lookAhead anyChar >>= \case
                     '{' -> inBraces
                     -- This is not as robust as the "qualif"
                     -- implementation, but considerably shorter.
                     --
                     -- This needs refactoring.
                     _   -> T.pack . concat <$> some (string "::" <|> some (satisfy variableAccept))
            when (v == "string") (fail "The special variable $string must not be used")
            return v

regexp :: Parser T.Text
regexp = do
    void (char '/')
    T.pack . concat <$> many ( do { void (char '\\') ; x <- anyChar; return ['\\', x] } <|> some (noneOf "/\\") )
        <* symbolic '/'

puppetArray :: Parser UValue
puppetArray = fmap (UArray . V.fromList) (brackets (expression `sepEndBy` comma)) <?> "Array"

puppetHash :: Parser UValue
puppetHash = fmap (UHash . V.fromList) (braces (hashPart `sepEndBy` comma)) <?> "Hash"
    where
        hashPart = (:!:) <$> (expression <* operator "=>")
                         <*> expression

puppetBool :: Parser Bool
puppetBool =  (reserved "true" >> return True)
          <|> (reserved "false" >> return False)
          <?> "Boolean"

resourceReferenceRaw :: Parser (T.Text, [Expression])
resourceReferenceRaw = do
    restype  <- resourceNameRef <?> "Resource reference type"
    resnames <- brackets (expression `sepBy1` comma) <?> "Resource reference values"
    return (restype, resnames)

resourceReference :: Parser UValue
resourceReference = do
    (restype, resnames) <- resourceReferenceRaw
    return $ UResourceReference restype $ case resnames of
                 [x] -> x
                 _   -> PValue (array resnames)

bareword :: Parser T.Text
bareword = identl (satisfy isAsciiLower) (satisfy acceptable) <?> "Bare word"
    where
        acceptable x = isAsciiLower x || isAsciiUpper x || isDigit x || (x == '_') || (x == '-')

-- The first argument defines if non-parenthesized arguments are acceptable
genFunctionCall :: Bool -> Parser (T.Text, V.Vector Expression)
genFunctionCall nonparens = do
    fname <- moduleName <?> "Function name"
    -- this is a hack. Contrary to what the documentation says,
    -- a "bareword" can perfectly be a qualified name :
    -- include foo::bar
    let argsc sep e = (fmap (PValue . UString) (qualif1 className) <|> e <?> "Function argument A") `sep` comma
        terminalF = terminalG (fail "function hack")
        expressionF = ParserT (buildExpressionParser expressionTable (unParser (token terminalF)) <?> "function expression")
        withparens = parens (argsc sepEndBy expression)
        withoutparens = argsc sepEndBy1 expressionF
    args  <- withparens <|> if nonparens
                                then withoutparens <?> "Function arguments B"
                                else fail "Function arguments C"
    return (fname, V.fromList args)

functionCall :: Parser UValue
functionCall = do
    (fname, args) <- genFunctionCall False
    return $ UFunctionCall fname args

literalValue :: Parser UValue
literalValue = token (fmap UString stringLiteral' <|> fmap UString bareword <|> fmap UNumber numericalvalue <?> "Literal Value")
    where
        numericalvalue = integerOrDouble >>= \case
            Left x -> return (fromIntegral x)
            Right y -> return (fromFloatDigits y)

-- this is a hack for functions :(
terminalG :: Parser Expression -> Parser Expression
terminalG g = parens expression
         <|> fmap (PValue . UInterpolable) interpolableString
         <|> (reserved "undef" *> return (PValue UUndef))
         <|> fmap PValue termRegexp
         <|> variable
         <|> fmap PValue puppetArray
         <|> fmap PValue puppetHash
         <|> fmap (PValue . UBoolean) puppetBool
         <|> fmap PValue resourceReference
         <|> g
         <|> fmap PValue literalValue

compileRegexp :: T.Text -> Parser Regex
compileRegexp p = case compile' compBlank execBlank (T.encodeUtf8 p) of
    Right r -> return r
    Left ms -> fail ("Can't parse regexp /" ++ T.unpack p ++ "/ : " ++ show ms)

termRegexp :: Parser UValue
termRegexp = do
    r <- regexp
    URegexp <$> pure r <*> compileRegexp r

terminal :: Parser Expression
terminal = terminalG (fmap PValue (fmap UHFunctionCall (try hfunctionCall) <|> try functionCall))

expression :: Parser Expression
expression = condExpression
             <|> ParserT (buildExpressionParser expressionTable (unParser (token terminal)))
             <?> "expression"
    where
        condExpression = do
            selectedExpression <- try (token terminal <* symbolic '?')
            let cas = do
                c <- (symbol "default" *> return SelectorDefault) -- default case
                        <|> fmap SelectorValue (fmap UVariableReference variableReference
                                                 <|> fmap UBoolean puppetBool
                                                 <|> literalValue
                                                 <|> fmap UInterpolable interpolableString
                                                 <|> termRegexp)
                void $ symbol "=>"
                e <- expression
                return (c :!: e)
            cases <- braces (cas `sepEndBy1` comma)
            return (ConditionalValue selectedExpression (V.fromList cases))

expressionTable :: [[Operator T.Text () Identity Expression]]
expressionTable = [ [ Postfix (chainl1 checkLookup (return (flip (.)))) ] -- http://stackoverflow.com/questions/10475337/parsec-expr-repeated-prefix-postfix-operator-not-supported
                  , [ Prefix ( operator' "-"   >> return Negate           ) ]
                  , [ Prefix ( operator' "!"   >> return Not              ) ]
                  , [ Infix  ( operator' "."   >> return FunctionApplication ) AssocLeft ]
                  , [ Infix  ( reserved' "in"  >> return Contains         ) AssocLeft ]
                  , [ Infix  ( operator' "/"   >> return Division         ) AssocLeft
                    , Infix  ( operator' "*"   >> return Multiplication   ) AssocLeft
                    ]
                  , [ Infix  ( operator' "+"   >> return Addition     ) AssocLeft
                    , Infix  ( operator' "-"   >> return Substraction ) AssocLeft
                    ]
                  , [ Infix  ( operator' "<<"  >> return LeftShift  ) AssocLeft
                    , Infix  ( operator' ">>"  >> return RightShift ) AssocLeft
                    ]
                  , [ Infix  ( operator' "=="  >> return Equal     ) AssocLeft
                    , Infix  ( operator' "!="  >> return Different ) AssocLeft
                    ]
                  , [ Infix  ( operator' "=~"  >> return RegexMatch    ) AssocLeft
                    , Infix  ( operator' "!~"  >> return NotRegexMatch ) AssocLeft
                    ]
                  , [ Infix  ( operator' ">="  >> return MoreEqualThan ) AssocLeft
                    , Infix  ( operator' "<="  >> return LessEqualThan ) AssocLeft
                    , Infix  ( operator' ">"   >> return MoreThan      ) AssocLeft
                    , Infix  ( operator' "<"   >> return LessThan      ) AssocLeft
                    ]
                  , [ Infix  ( reserved' "and" >> return And ) AssocLeft
                    , Infix  ( reserved' "or"  >> return Or  ) AssocLeft
                    ]
                  ]
    where
        checkLookup :: OP (Expression -> Expression)
        checkLookup = flip Lookup <$> unParser (between (operator "[") (operator "]") expression)
        operator' :: String -> OP ()
        operator' = unParser . operator
        reserved' :: String -> OP ()
        reserved' = unParser . reserved

stringExpression :: Parser Expression
stringExpression = fmap (PValue . UInterpolable) interpolableString <|> (reserved "undef" *> return (PValue UUndef)) <|> fmap (PValue . UBoolean) puppetBool <|> variable <|> fmap PValue literalValue

variableAssignment :: Parser [Statement]
variableAssignment = do
    p <- getPosition
    v <- variableReference
    void $ symbolic '='
    e <- expression
    when (T.all isDigit v) (fail "Can't assign fully numeric variables")
    pe <- getPosition
    return [VariableAssignment (VarAss v e (p :!: pe))]

nodeStmt :: Parser [Statement]
nodeStmt = do
    p <- getPosition
    reserved "node"
    let nm (URegexp nn nr) = return (NodeMatch nn nr)
        nm _ = fail "? can't happen, termRegexp didn't return a URegexp ?"
        toString (UString s) = s
        toString (UNumber n) = scientific2text n
        toString _ = error "Can't happen at nodeStmt"
        nodename = (reserved "default" >> return NodeDefault) <|> fmap (NodeName . toString) literalValue
    ns <- ((termRegexp >>= nm) <|> nodename) `sepBy1` comma
    inheritance <- option S.Nothing (fmap S.Just (reserved "inherits" *> nodename))
    st <- braces statementList
    pe <- getPosition
    return [Node (Nd n st inheritance (p :!: pe)) | n <- ns]

puppetClassParameters :: Parser (V.Vector (Pair T.Text (S.Maybe Expression)))
puppetClassParameters = V.fromList <$> parens (var `sepEndBy` comma)
    where
        toStrictMaybe (Just x) = S.Just x
        toStrictMaybe Nothing  = S.Nothing
        var :: Parser (Pair T.Text (S.Maybe Expression))
        var = (:!:)
                <$> variableReference
                <*> (toStrictMaybe <$> optional (symbolic '=' *> expression))

defineStmt :: Parser [Statement]
defineStmt = do
    p <- getPosition
    reserved "define"
    name <- typeName
    -- TODO check native type
    params <- option V.empty puppetClassParameters
    st <- braces statementList
    pe <- getPosition
    return [DefineDeclaration (DefineDec name params st (p :!: pe))]

puppetIfStyleCondition :: Parser (Pair Expression (V.Vector Statement))
puppetIfStyleCondition = (:!:) <$> expression <*> braces statementList

unlessCondition :: Parser [Statement]
unlessCondition = do
    p <- getPosition
    reserved "unless"
    (cond :!: stmts) <- puppetIfStyleCondition
    pe <- getPosition
    return [ConditionalStatement (CondStatement (V.singleton (Not cond :!: stmts)) (p :!: pe))]

ifCondition :: Parser [Statement]
ifCondition = do
    p <- getPosition
    reserved "if"
    maincond <- puppetIfStyleCondition
    others   <- many (reserved "elsif" *> puppetIfStyleCondition)
    elsecond <- option V.empty (reserved "else" *> braces statementList)
    let ec = if V.null elsecond
                 then []
                 else [PValue (UBoolean True) :!: elsecond]
    pe <- getPosition
    return [ ConditionalStatement (CondStatement (V.fromList (maincond : others ++ ec)) (p :!: pe)) ]

caseCondition :: Parser [Statement]
caseCondition = do
    let puppetRegexpCase = do
            reg <- termRegexp
            void $ symbolic ':'
            stmts <- braces statementList
            return [ (PValue reg, stmts) ]
        defaultCase = do
            try (reserved "default")
            void $ symbolic ':'
            stmts <- braces statementList
            return [ (PValue (UBoolean True), stmts) ]
        puppetCase = do
            compares <- expression `sepBy1` comma
            void $ symbolic ':'
            stmts <- braces statementList
            return $ map (,stmts) compares
        condsToExpression e (x, stmts) = f x :!: stmts
            where f = case x of
                          (PValue (UBoolean _))  -> id
                          (PValue (URegexp _ _)) -> RegexMatch e
                          _                      -> Equal e
    p <- getPosition
    reserved "case"
    expr1 <- expression
    condlist <- braces (some (puppetRegexpCase <|> defaultCase <|> puppetCase))
    pe <- getPosition
    return [ ConditionalStatement (CondStatement (V.fromList (map (condsToExpression expr1) (concat condlist))) (p :!: pe) ) ]

data OperatorChain a = OperatorChain a LinkType (OperatorChain a)
                     | EndOfChain a

instance F.Foldable OperatorChain where
    foldMap f (EndOfChain x) = f x
    foldMap f (OperatorChain a _ nx) = f a <> F.foldMap f nx

operatorChainStatement :: OperatorChain a -> a
operatorChainStatement (OperatorChain a _ _) = a
operatorChainStatement (EndOfChain x) = x

zipChain :: OperatorChain a -> [ ( a, a, LinkType ) ]
zipChain (OperatorChain a d nx) = (a, operatorChainStatement nx, d) : zipChain nx
zipChain (EndOfChain _) = []

depOperator :: Parser LinkType
depOperator =   (operator "->" *> pure RBefore)
            <|> (operator "~>" *> pure RNotify)

-- | Used to parse chains of resource relations
parseRelationships :: Parser a -> Parser (OperatorChain a)
parseRelationships p = do
    g <- p
    o <- optional depOperator
    case o of
        Just o' -> OperatorChain g o' <$> parseRelationships p
        Nothing -> pure (EndOfChain g)

statementRelationships :: Parser [ResDec] -> Parser [Statement]
statementRelationships p = do
    rels <- parseRelationships p
    let relations = do
            (g1, g2, lt) <- zipChain rels
            ResDec rt1 rn1 _ _ (_ :!: pe1) <- g1
            ResDec rt2 rn2 _ _ (ps2 :!: _) <- g2
            return (Dep (rt1 :!: rn1) (rt2 :!: rn2) lt (pe1 :!: ps2))
    return $ map ResourceDeclaration (mconcat (F.toList rels)) <> map Dependency relations

startDepChains :: Position -> T.Text -> [Expression] -> Parser [Dep]
startDepChains p restype resnames = do
    d <- depOperator
    groups <- zipChain . OperatorChain (restype, resnames) d <$> parseRelationships resourceReferenceRaw
    pe <- getPosition
    return $ do
        ((rt, rns), (dt, dns), lt) <- groups
        rn <- rns
        dn <- dns
        return (Dep (rt :!: rn) (dt :!: dn) lt (p :!: pe))

rrGroupRef :: Position -> T.Text -> Parser [Statement]
rrGroupRef p restype = do
    resnames <- brackets (expression `sepBy1` comma) <?> "Resource reference values"
    fmap (map Dependency) (startDepChains p restype resnames) <|> resourceOverride p restype resnames

resourceGroup :: Parser [Statement]
resourceGroup = statementRelationships resourceGroup'

resourceGroup' :: Parser [ResDec]
resourceGroup' = do
    let resourceName = token stringExpression
        resourceDeclaration = do
            p <- getPosition
            names <- brackets (resourceName `sepEndBy1` comma) <|> fmap return resourceName
            void $ symbolic ':'
            vals  <- fmap V.fromList (assignment `sepEndBy` comma)
            pe <- getPosition
            return [(n, vals, p :!: pe) | n <- names ]
        groupDeclaration = (,) <$> many (char '@') <*> typeName <* symbolic '{'
    (virts, rtype) <- try groupDeclaration -- for matching reasons, this gets a try until the opening brace
    x <- resourceDeclaration `sepEndBy` (symbolic ';' <|> comma)
    void $ symbolic '}'
    virtuality <- case virts of
                      ""   -> return Normal
                      "@"  -> return Virtual
                      "@@" -> return Exported
                      _    -> fail "Invalid virtuality"
    return [ ResDec rtype rname conts virtuality pos | (rname, conts, pos) <- concat x ]

assignment :: Parser (Pair T.Text Expression)
assignment = (:!:) <$> bw <*> (symbol "=>" *> expression)
    where
        bw = identl (satisfy isAsciiLower) (satisfy acceptable) <?> "Assignment key"
        acceptable x = isAsciiLower x || isAsciiUpper x || isDigit x || (x == '_') || (x == '-')

resourceDefaults :: Position -> T.Text -> Parser [Statement]
resourceDefaults p rnd = do
    let assignmentList = V.fromList <$> assignment `sepEndBy1` comma
    asl <- braces assignmentList
    pe <- getPosition
    return [DefaultDeclaration (DefaultDec rnd asl (p :!: pe))]

resourceOverride :: Position -> T.Text -> [Expression] ->  Parser [Statement]
resourceOverride p restype names = do
    assignments <- V.fromList <$> braces (assignment `sepEndBy` comma)
    pe <- getPosition
    return [ ResourceOverride (ResOver restype n assignments (p :!: pe)) | n <- names ]

-- TODO
searchExpression :: Parser SearchExpression
searchExpression = parens searchExpression <|> check <|> combine
    where
        combine = do
            e1  <- parens searchExpression <|> check
            opr <- (operator "and" *> return AndSearch) <|> (operator "or" *> return OrSearch)
            e2  <- searchExpression
            return (opr e1 e2)
        check = do
            attrib <- parameterName
            opr    <- (operator "==" *> return EqualitySearch) <|> (operator "!=" *> return NonEqualitySearch)
            term   <- stringExpression
            return (opr attrib term)

resourceCollection :: Position -> T.Text -> Parser [Statement]
resourceCollection p restype = do
    openchev <- some (char '<')
    when (length openchev > 2) (fail "Too many brackets")
    void $ symbolic '|'
    e <- option AlwaysTrue searchExpression
    void (char '|')
    void (count (length openchev) (char '>'))
    someSpace
    overrides <- option [] $ braces (assignment `sepEndBy` comma)
    let collectortype = if length openchev == 1
                            then Collector
                            else ExportedCollector
    pe <- getPosition
    return [ ResourceCollection (RColl collectortype restype e (V.fromList overrides) (p :!: pe) ) ]

classDefinition :: Parser [Statement]
classDefinition = do
    p <- getPosition
    reserved "class"
    x <- ClassDecl <$> className
                   <*> option V.empty puppetClassParameters
                   <*> option S.Nothing (fmap S.Just (reserved "inherits" *> className))
                   <*> braces statementList
                   <*> ( (p :!:) <$> getPosition )
    return [ClassDeclaration x]

mainFunctionCall :: Parser [Statement]
mainFunctionCall = do
    p <- getPosition
    (fname, args) <- genFunctionCall True
    pe <- getPosition
    return [ MainFunctionCall (MFC fname args (p :!: pe)) ]

rrGroup :: Parser [Statement]
rrGroup = do
    p <- getPosition
    restype  <- resourceNameRef
    lookAhead anyChar >>= \case
        '[' -> rrGroupRef p restype <?> "What comes after a resource reference"
        _   -> resourceDefaults p restype <|> resourceCollection p restype <?> "What comes after a resource type"

mainHFunctionCall :: Parser [Statement]
mainHFunctionCall = do
    p <- getPosition
    fc <- try hfunctionCall
    pe <- getPosition
    return [SHFunctionCall (SFC fc (p :!: pe))]

dotCall :: Parser [Statement]
dotCall = do
    p <- getPosition
    ex <- expression
    pe <- getPosition
    hf <- case ex of
              FunctionApplication e (PValue (UHFunctionCall hf)) -> do
                  unless (S.isNothing (hf ^. hfexpr)) (fail "Can't call a function with . and ()")
                  return (hf & hfexpr .~ S.Just e)
              PValue (UHFunctionCall hf) -> do
                  when (S.isNothing (hf ^. hfexpr)) (fail "This function needs data to operate on")
                  return hf
              _ -> fail "A method chained by dots."
    unless (hf ^. hftype == HFEach) (fail "Expected 'each', the other types of method calls are not supported by language-puppet at the statement level.")
    return [SHFunctionCall (SFC hf (p :!: pe))]

statement :: Parser [Statement]
statement =
        try dotCall
    <|> variableAssignment
    <|> nodeStmt
    <|> defineStmt
    <|> unlessCondition
    <|> ifCondition
    <|> caseCondition
    <|> resourceGroup
    <|> rrGroup
    <|> classDefinition
    <|> mainHFunctionCall
    <|> mainFunctionCall
    <?> "Statement"


statementList :: Parser (V.Vector Statement)
statementList = fmap (V.fromList . concat) (many statement)

puppetParser :: Parser (V.Vector Statement)
puppetParser = someSpace >> statementList

{-
- Stuff related to the new functions with "lambdas"
-}

parseHFunction :: Parser HigherFuncType
parseHFunction =   (reserved "each"   *> pure HFEach)
               <|> (reserved "map"    *> pure HFMap )
               <|> (reserved "reduce" *> pure HFReduce)
               <|> (reserved "filter" *> pure HFFilter)
               <|> (reserved "slice"  *> pure HFSlice)

parseHParams :: Parser BlockParameters
parseHParams = between (symbolic '|') (symbolic '|') hp
    where
        acceptablePart = T.pack <$> ident identifierStyle
        hp = do
            vars <- (char '$' *> acceptablePart) `sepBy1` comma
            case vars of
                [a] -> return (BPSingle a)
                [a,b] -> return (BPPair a b)
                _ -> fail "Invalid number of variables between the pipes"

hfunctionCall :: Parser HFunctionCall
hfunctionCall = do
    let toStrict (Just x) = S.Just x
        toStrict Nothing  = S.Nothing
    HFunctionCall <$> parseHFunction
                  <*> fmap (toStrict . join) (optional (parens (optional expression)))
                  <*> parseHParams
                  <*> (symbolic '{' *> fmap (V.fromList . concat) (many (try statement)))
                  <*> fmap toStrict (optional expression) <* symbolic '}'
