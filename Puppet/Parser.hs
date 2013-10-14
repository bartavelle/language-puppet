module Puppet.Parser (puppetParser,expression) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashSet as HS
import qualified Data.Maybe.Strict as S
import Data.Tuple.Strict hiding (fst,zip)
import Text.Regex.PCRE.String

import Data.Char
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative

import Puppet.Parser.Types

import Text.Parsec.Expr
import Text.Parser.Token hiding (stringLiteral')
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.LookAhead
import Text.Parser.Parsec ()
import Text.Parser.Token.Highlight
import Text.Parsec.Prim (getPosition, ParsecT)
import Text.Parsec.Text ()

type Parser = ParsecT T.Text () IO

stringLiteral' :: Parser T.Text
stringLiteral' = char '\'' *> interior <* symbolic '\''
    where
        interior = fmap (T.pack . concat) $ many (some (noneOf "'\\") <|> (char '\\' *> fmap escape anyChar))
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
    let acceptablePart = fmap T.pack (ident identifierStyle)
    out <- qualif acceptablePart
    when (out == "string") (fail "The special variable $string should never be used")
    return out

qualif :: Parser T.Text -> Parser T.Text
qualif p = token $ do
    header <- option "" (try (string "::"))
    rest <- fmap (T.intercalate "::") (p `sepBy1` try (string "::"))
    return (T.append (T.pack header) rest)

qualif1 :: Parser T.Text -> Parser T.Text
qualif1 p = try $ do
    r <- qualif p
    if "::" `T.isInfixOf` r
        then return r
        else fail "This parser is not qualified"

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
inBraces =  between (char '{') (char '}') (fmap T.pack (some (satisfy (/= '}'))))

variableReference :: Parser T.Text
variableReference = do
    void (char '$')
    v <- lookAhead anyChar >>= \case
         '{' -> inBraces
         _   -> variableName
    when (v == "string") (fail "The special variable $string must not be used")
    return v

interpolableString :: Parser (V.Vector UValue)
interpolableString = fmap V.fromList $ between (char '"') (symbolic '"') $
    many (fmap UVariableReference interpolableVariableReference <|> doubleQuotedStringContent <|> fmap (UString . T.singleton) (char '$'))
    where
        doubleQuotedStringContent = fmap (UString . T.pack . concat) $
            some ((char '\\' *> anyChar >>= stringEscape) <|> some (noneOf "\"\\$"))
        stringEscape :: Char -> Parser String
        stringEscape 'n' = return "\n"
        stringEscape 't' = return "\t"
        stringEscape 'r' = return "\r"
        stringEscape '"' = return "\""
        stringEscape '\\' = return "\\"
        stringEscape '$' = return "$"
        stringEscape x = fail $ "unknown escape pattern \\" ++ [x]
        -- this is specialized because we can't be "tokenized" here
        variableAccept x = isAsciiLower x || isAsciiUpper x || isDigit x || x == '_'
        interpolableVariableReference = do
            void (char '$')
            v <- lookAhead anyChar >>= \case
                     '{' -> inBraces
                     -- This is not as robust as the "qualif"
                     -- implementation, but considerably shorter.
                     --
                     -- This needs refactoring.
                     _   -> fmap (T.pack . concat) (some (string "::" <|> some (satisfy variableAccept)))
            when (v == "string") (fail "The special variable $string must not be used")
            return v

regexp :: Parser T.Text
regexp = do
    void (char '/')
    v <- many ( do { void (char '\\') ; x <- anyChar; return ['\\', x] } <|> some (noneOf "/\\") )
    void $ symbolic '/'
    return $! T.pack $! concat v

variableOrHash :: Parser Expression
variableOrHash = do
    varname <- variableReference <?> "Variable reference"
    -- chained lookups are resolved here
    hr <- many (brackets expression)
    return $! foldl Lookup (PValue (UVariableReference varname)) hr

puppetArray :: Parser UValue
puppetArray = fmap (UArray . V.fromList) (brackets (expression `sepEndBy` comma)) <?> "Array"

puppetHash :: Parser UValue
puppetHash = fmap (UHash . V.fromList) (braces (hashPart `sepEndBy` comma)) <?> "Hash"
    where
        hashPart = do
            -- a special case for "default" because of the ? selector ...
            a <- expression
            void $ operator "=>"
            b <- expression
            return (a :!: b)

puppetBool :: Parser Bool
puppetBool = (reserved "true" >> return True) <|> (reserved "false" >> return False) <?> "Boolean"

resourceReferenceRaw :: Parser (T.Text, [Expression])
resourceReferenceRaw = do
    restype  <- resourceNameRef <?> "Resource reference type"
    resnames <- brackets (expression `sepBy1` comma) <?> "Resource reference values"
    return (restype, resnames)

resourceReference :: Parser UValue
resourceReference = do
    (restype, resnames) <- resourceReferenceRaw
    return $ case resnames of
                 [x] -> UResourceReference restype x
                 _   -> UResourceReference restype (PValue (array resnames))

bareword :: Parser T.Text
bareword = identl (satisfy isAsciiLower) (satisfy acceptable) <?> "Bare word"
    where
        acceptable x = isAsciiLower x || isAsciiUpper x || isDigit x || (x == '_') || (x == '-')

genFunctionCall :: Parser (T.Text, V.Vector Expression)
genFunctionCall = do
    fname <- moduleName <?> "Function name"
    -- this is a hack. Contrary to what the documentation says,
    -- a "bareword" can perfectly be a qualified name :
    -- include foo::bar
    let argsc e = (fmap (PValue . UString) (qualif1 className) <|> e <?> "Function argument") `sepEndBy1` comma
        terminalF = terminalG (fail "function hack")
        expressionF = buildExpressionParser expressionTable (token terminalF) <?> "function expression"
    args  <- parens (argsc expression) <|> argsc expressionF <?> "Function arguments"
    return (fname, V.fromList args)

functionCall :: Parser UValue
functionCall = do
    (fname, args) <- genFunctionCall
    return $ UFunctionCall fname args

literalValue :: Parser T.Text
literalValue = token (stringLiteral' <|> bareword <|> numericalvalue <?> "Literal Value")
    where
        numericalvalue = integerOrDouble >>= \case
            Left x -> return (T.pack $ show x)
            Right y -> return (T.pack $ show y)

-- this is a hack for functions :(
terminalG :: Parser Expression -> Parser Expression
terminalG g = parens expression
         <|> fmap (PValue . UInterpolable) interpolableString
         <|> (reserved "undef" *> return (PValue UUndef))
         <|> fmap PValue termRegexp
         <|> variableOrHash
         <|> fmap PValue puppetArray
         <|> fmap PValue puppetHash
         <|> fmap (PValue . UBoolean) puppetBool
         <|> fmap PValue resourceReference
         <|> g
         <|> fmap (PValue . UString) literalValue

compileRegexp :: T.Text -> Parser Regex
compileRegexp p = (liftIO . compile compBlank execBlank . T.unpack) p >>= \case
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
             <|> buildExpressionParser expressionTable (token terminal)
             <?> "expression"
    where
        condExpression = do
            selectedExpression <- try (token terminal <* symbolic '?')
            let cas = do
                c <- (symbol "default" *> return SelectorDefault) -- default case
                        <|> fmap SelectorValue (fmap UVariableReference variableReference
                                                 <|> fmap UString literalValue
                                                 <|> fmap UInterpolable interpolableString
                                                 <|> termRegexp)
                void $ symbol "=>"
                e <- expression
                return (c :!: e)
            cases <- braces (cas `sepEndBy1` comma)
            return (ConditionalValue selectedExpression (V.fromList cases))

expressionTable :: [[Operator T.Text () IO Expression]]
expressionTable = [ -- [ Infix  ( operator "?"   >> return ConditionalValue ) AssocLeft ]
                    [ Prefix ( operator "-"   >> return Negate           ) ]
                  , [ Prefix ( operator "!"   >> return Not              ) ]
                  , [ Infix  ( operator "."   >> return FunctionApplication ) AssocLeft ]
                  , [ Infix  ( reserved "in"  >> return Contains         ) AssocLeft ]
                  , [ Infix  ( operator "/"   >> return Division         ) AssocLeft
                    , Infix  ( operator "*"   >> return Multiplication   ) AssocLeft
                    ]
                  , [ Infix  ( operator "+"   >> return Addition     ) AssocLeft
                    , Infix  ( operator "-"   >> return Substraction ) AssocLeft
                    ]
                  , [ Infix  ( operator "<<"  >> return LeftShift  ) AssocLeft
                    , Infix  ( operator ">>"  >> return RightShift ) AssocLeft
                    ]
                  , [ Infix  ( operator "=="  >> return Equal     ) AssocLeft
                    , Infix  ( operator "!="  >> return Different ) AssocLeft
                    ]
                  , [ Infix  ( operator ">"   >> return MoreThan      ) AssocLeft
                    , Infix  ( operator ">="  >> return MoreEqualThan ) AssocLeft
                    , Infix  ( operator "<="  >> return LessEqualThan ) AssocLeft
                    , Infix  ( operator "<"   >> return LessThan      ) AssocLeft
                    ]
                  , [ Infix  ( reserved "and" >> return And ) AssocLeft
                    , Infix  ( reserved "or"  >> return Or  ) AssocLeft
                    ]
                  , [ Infix  ( operator "=~"  >> return RegexMatch    ) AssocLeft
                    , Infix  ( operator "!~"  >> return NotRegexMatch ) AssocLeft
                    ]
                  ]

stringExpression :: Parser Expression
stringExpression = fmap (PValue . UInterpolable) interpolableString <|> (reserved "undef" *> return (PValue UUndef)) <|> fmap (PValue . UBoolean) puppetBool <|> variableOrHash <|> fmap (PValue . UString) literalValue

variableAssignment :: Parser [Statement]
variableAssignment = do
    p <- getPosition
    v <- variableReference
    void $ symbolic '='
    e <- expression
    when (T.all isDigit v) (fail "Can't assign fully numeric variables")
    pe <- getPosition
    return [VariableAssignment v e (p :!: pe)]

nodeStmt :: Parser [Statement]
nodeStmt = do
    p <- getPosition
    reserved "node"
    let nm (URegexp nn nr) = return (NodeMatch nn nr)
        nm _ = fail "? can't happen, termRegexp didn't return a URegexp ?"
    let nodename = (reserved "default" >> return NodeDefault) <|> fmap NodeName literalValue
    ns <- ((termRegexp >>= nm) <|> nodename) `sepBy1` comma
    inheritance <- option S.Nothing (fmap S.Just (reserved "inherits" *> nodename))
    st <- braces statementList
    pe <- getPosition
    return [Node n st inheritance (p :!: pe) | n <- ns]

puppetClassParameters :: Parser (V.Vector (Pair T.Text (S.Maybe Expression)))
puppetClassParameters = fmap V.fromList $ parens (var `sepBy` comma)
    where
        toStrictMaybe (Just x) = S.Just x
        toStrictMaybe Nothing  = S.Nothing
        var :: Parser (Pair T.Text (S.Maybe Expression))
        var = do
            vname <- variableReference
            value <- fmap toStrictMaybe $ optional (symbolic '=' *> expression)
            return $ vname :!: value

defineStmt :: Parser [Statement]
defineStmt = do
    p <- getPosition
    reserved "define"
    name <- typeName
    -- TODO check native type
    params <- option V.empty puppetClassParameters
    st <- braces statementList
    pe <- getPosition
    return [DefineDeclaration name params st (p :!: pe)]

puppetIfStyleCondition :: Parser (Pair Expression (V.Vector Statement))
puppetIfStyleCondition = (:!:) <$> expression <*> braces statementList

unlessCondition :: Parser [Statement]
unlessCondition = do
    p <- getPosition
    reserved "unless"
    (cond :!: stmts) <- puppetIfStyleCondition
    pe <- getPosition
    return [ConditionalStatement (V.singleton (Not cond :!: stmts)) (p :!: pe)]

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
    return [ ConditionalStatement (V.fromList (maincond : others ++ ec)) (p :!: pe) ]

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
            return [ (cmp, stmts) | cmp <- compares ]
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
    return [ ConditionalStatement (V.fromList (map (condsToExpression expr1) (concat condlist))) (p :!: pe) ]

resourceGroup :: Parser [Statement]
resourceGroup = do
    groups <- resourceGroup' `sepBy1` operator "->"
    let relations = do
        (g1, g2) <- zip groups (tail groups)
        ResourceDeclaration rt1 rn1 _ _ (_ :!: pe1) <- g1
        ResourceDeclaration rt2 rn2 _ _ (ps2 :!: _) <- g2
        return (Dependency (rt1 :!: rn1) (rt2 :!: rn2) (pe1 :!: ps2))
    return $ concat groups ++ relations

resourceGroup' :: Parser [Statement]
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
    return [ ResourceDeclaration rtype rname conts virtuality pos | (rname, conts, pos) <- concat x ]

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
    return [DefaultDeclaration rnd asl (p :!: pe)]

resourceOverride :: Position -> T.Text -> [Expression] ->  Parser [Statement]
resourceOverride p restype names = do
    assignments <- fmap V.fromList $ braces (assignment `sepEndBy` comma)
    pe <- getPosition
    return [ ResourceOverride restype n assignments (p :!: pe) | n <- names ]

-- TODO
searchExpression :: Parser SearchExpression
searchExpression = parens searchExpression <|> check <|> combine
    where
        combine = do
            e1 <- parens searchExpression <|> check
            op <- (operator "and" *> return AndSearch) <|> (operator "or" *> return OrSearch)
            e2 <- searchExpression
            return (op e1 e2)
        check = do
            attrib <- parameterName
            op <- (operator "==" *> return EqualitySearch) <|> (operator "!=" *> return NonEqualitySearch)
            term <- stringExpression
            return (op attrib term)

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
    return [ ResourceCollection collectortype restype e (V.fromList overrides) (p :!: pe) ]

classDefinition :: Parser [Statement]
classDefinition = do
    p <- getPosition
    reserved "class"
    classname <- className
    params <- option V.empty puppetClassParameters
    inheritance <- option S.Nothing (fmap S.Just (reserved "inherits" *> className))
    st <- braces statementList
    pe <- getPosition
    return [ ClassDeclaration classname  params inheritance st (p :!: pe) ]

mainFunctionCall :: Parser [Statement]
mainFunctionCall = do
    p <- getPosition
    (fname, args) <- genFunctionCall
    pe <- getPosition
    return [ MainFunctionCall fname args (p :!: pe) ]

startDepChains :: Position -> T.Text -> [Expression] -> Parser [Statement]
startDepChains p restype resnames = do
    operator "->"
    -- FIXME positions
    nxts <- resourceReferenceRaw `sepBy` operator "->"
    pe <- getPosition
    let refs = (restype, resnames) : nxts
    return [ Dependency (rt :!: rn) (dt :!: dn) (p :!: pe) | ((rt, rns), (dt,dns)) <- zip refs (tail refs), rn <- rns, dn <- dns ]

rrGroupRef :: Position -> T.Text -> Parser [Statement]
rrGroupRef p restype = do
    resnames <- brackets (expression `sepBy1` comma) <?> "Resource reference values"
    startDepChains p restype resnames <|> resourceOverride p restype resnames

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
    return [SHFunctionCall fc (p :!: pe)]

statement :: Parser [Statement]
statement =
    variableAssignment
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
        hp = do
            vars <- variableReference `sepBy1` comma
            case vars of
                [a] -> return (BPSingle a)
                [a,b] -> return (BPPair a b)
                _ -> fail "Invalid number of variables between the pipes"

hfunctionCall :: Parser HFunctionCall
hfunctionCall = do
    let toStrict (Just x) = S.Just x
        toStrict Nothing  = S.Nothing
    HFunctionCall <$> parseHFunction
                  <*> fmap toStrict (optional (parens expression))
                  <*> parseHParams
                  <*> (symbolic '{' *> fmap (V.fromList . concat) (many statement))
                  <*> fmap toStrict (optional expression) <* symbolic '}'

