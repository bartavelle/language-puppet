{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-| Parse puppet source code from text. -}
module Puppet.Parser (
  -- * Runner
    runPParser
  -- * Parsers
  , puppetParser
  , expression
) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.HashSet as HS
import qualified Data.Maybe.Strict as S
import qualified Data.Foldable as F
import           Data.Tuple.Strict hiding (fst,zip)
import           Text.Regex.PCRE.ByteString.Utils

import           Data.Char
import           Control.Monad
import           Control.Applicative
import           Control.Lens hiding (noneOf)

import           Puppet.Parser.Types
import           Puppet.Utils

import           Data.Scientific
import           Text.Parsec.Error (ParseError)
import           Text.Parsec.Expr
import           Text.Parsec.Pos (SourcePos,SourceName)
import qualified Text.Parsec.Prim as PP
import           Text.Parsec.Text ()
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token hiding (stringLiteral')
import           Text.Parser.Token.Highlight

-- | Run a puppet parser against some 'T.Text' input.
runPParser :: Parser a -> SourceName -> T.Text -> Either ParseError a
runPParser (ParserT p) = PP.parse p

-- | Parse a collection of puppet 'Statement'.
puppetParser :: Parser (V.Vector Statement)
puppetParser = someSpace >> statementList

-- | Parse a puppet 'Expression'.
expression :: Parser Expression
expression = condExpression
             <|> ParserT (buildExpressionParser expressionTable (unParser (token terminal)))
             <?> "expression"
    where
        condExpression = do
            selectedExpression <- try (token terminal <* symbolic '?')
            let cas = do
                c <- (SelectorDefault <$ symbol "default") -- default case
                        <|> fmap SelectorValue (fmap UVariableReference variableReference
                                                 <|> fmap UBoolean puppetBool
                                                 <|> (UUndef <$ symbol "undef")
                                                 <|> literalValue
                                                 <|> fmap UInterpolable interpolableString
                                                 <|> (URegexp <$> termRegexp))
                void $ symbol "=>"
                e <- expression
                return (c :!: e)
            cases <- braces (cas `sepEndBy1` comma)
            return (ConditionalValue selectedExpression (V.fromList cases))

newtype Parser a = ParserT { unParser :: PP.ParsecT T.Text () Identity a}
                 deriving (Functor, Applicative, Alternative)

deriving instance Monad Parser
deriving instance Parsing Parser
deriving instance CharParsing Parser
deriving instance LookAheadParsing Parser

getPosition :: Parser SourcePos
getPosition = ParserT PP.getPosition

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
variable = Terminal . UVariableReference <$> variableReference

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
    let acceptablePart = T.pack <$> many (satisfy identifierAcceptable)
        identifierAcceptable x = isAsciiLower x || isAsciiUpper x || isDigit x || (x == '_')
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

variableReference :: Parser T.Text
variableReference = do
    void (char '$')
    v <- variableName
    when (v == "string") (fail "The special variable $string must not be used")
    return v

interpolableString :: Parser (V.Vector Expression)
interpolableString = V.fromList <$> between (char '"') (symbolic '"')
     ( many (interpolableVariableReference <|> doubleQuotedStringContent <|> fmap (Terminal . UString . T.singleton) (char '$')) )
    where
        doubleQuotedStringContent = Terminal . UString . T.pack . concat <$>
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
        rvariableName = do
            v <- T.pack . concat <$> some (string "::" <|> some (satisfy variableAccept))
            when (v == "string") (fail "The special variable $string must not be used")
            return v
        rvariable = Terminal . UVariableReference <$> rvariableName
        simpleIndexing = Lookup <$> rvariable <*> between (symbolic '[') (symbolic ']') expression
        interpolableVariableReference = try $ do
            void (char '$')
            lookAhead anyChar >>= \c -> case c of
                 '{' -> between (symbolic '{') (char '}') (   try simpleIndexing
                                                          <|> rvariable
                                                          )
                 -- This is not as robust as the "qualif"
                 -- implementation, but considerably shorter.
                 --
                 -- This needs refactoring.
                 _   -> rvariable

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
                 _   -> Terminal $ UArray (V.fromList resnames)

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
    let argsc sep e = (fmap (Terminal . UString) (qualif1 className) <|> e <?> "Function argument A") `sep` comma
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
        numericalvalue = integerOrDouble >>= \i -> case i of
            Left x -> return (fromIntegral x)
            Right y -> return (fromFloatDigits y)

-- this is a hack for functions :(
terminalG :: Parser Expression -> Parser Expression
terminalG g = parens expression
         <|> fmap (Terminal . UInterpolable) interpolableString
         <|> (reserved "undef" *> return (Terminal UUndef))
         <|> fmap (Terminal . URegexp) termRegexp
         <|> variable
         <|> fmap Terminal puppetArray
         <|> fmap Terminal puppetHash
         <|> fmap (Terminal . UBoolean) puppetBool
         <|> fmap Terminal resourceReference
         <|> g
         <|> fmap Terminal literalValue

compileRegexp :: T.Text -> Parser CompRegex
compileRegexp p = case compile' compBlank execBlank (T.encodeUtf8 p) of
    Right r -> return $ CompRegex p r
    Left ms -> fail ("Can't parse regexp /" ++ T.unpack p ++ "/ : " ++ show ms)

termRegexp :: Parser CompRegex
termRegexp = regexp >>= compileRegexp

terminal :: Parser Expression
terminal = terminalG (fmap Terminal (fmap UHFunctionCall (try hfunctionCall) <|> try functionCall))



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
stringExpression = fmap (Terminal . UInterpolable) interpolableString <|> (reserved "undef" *> return (Terminal UUndef)) <|> fmap (Terminal . UBoolean) puppetBool <|> variable <|> fmap Terminal literalValue

variableAssignment :: Parser VarAss
variableAssignment = do
    p <- getPosition
    v <- variableReference
    void $ symbolic '='
    e <- expression
    when (T.all isDigit v) (fail "Can't assign fully numeric variables")
    pe <- getPosition
    return (VarAss v e (p :!: pe))

nodeStmt :: Parser [Nd]
nodeStmt = do
    p <- getPosition
    reserved "node"
    let toString (UString s) = s
        toString (UNumber n) = scientific2text n
        toString _ = error "Can't happen at nodeStmt"
        nodename = (reserved "default" >> return NodeDefault) <|> fmap (NodeName . toString) literalValue
    ns <- (fmap NodeMatch termRegexp <|> nodename) `sepBy1` comma
    inheritance <- option S.Nothing (fmap S.Just (reserved "inherits" *> nodename))
    st <- braces statementList
    pe <- getPosition
    return [Nd n st inheritance (p :!: pe) | n <- ns]

puppetClassParameters :: Parser (V.Vector (Pair T.Text (S.Maybe Expression)))
puppetClassParameters = V.fromList <$> parens (var `sepEndBy` comma)
    where
        toStrictMaybe (Just x) = S.Just x
        toStrictMaybe Nothing  = S.Nothing
        var :: Parser (Pair T.Text (S.Maybe Expression))
        var = (:!:)
                <$> variableReference
                <*> (toStrictMaybe <$> optional (symbolic '=' *> expression))

defineStmt :: Parser DefineDec
defineStmt = do
    p <- getPosition
    reserved "define"
    name <- typeName
    -- TODO check native type
    params <- option V.empty puppetClassParameters
    st <- braces statementList
    pe <- getPosition
    return (DefineDec name params st (p :!: pe))

puppetIfStyleCondition :: Parser (Pair Expression (V.Vector Statement))
puppetIfStyleCondition = (:!:) <$> expression <*> braces statementList

unlessCondition :: Parser CondStatement
unlessCondition = do
    p <- getPosition
    reserved "unless"
    (cond :!: stmts) <- puppetIfStyleCondition
    pe <- getPosition
    return (CondStatement (V.singleton (Not cond :!: stmts)) (p :!: pe))

ifCondition :: Parser CondStatement
ifCondition = do
    p <- getPosition
    reserved "if"
    maincond <- puppetIfStyleCondition
    others   <- many (reserved "elsif" *> puppetIfStyleCondition)
    elsecond <- option V.empty (reserved "else" *> braces statementList)
    let ec = if V.null elsecond
                 then []
                 else [Terminal (UBoolean True) :!: elsecond]
    pe <- getPosition
    return (CondStatement (V.fromList (maincond : others ++ ec)) (p :!: pe))

caseCondition :: Parser CondStatement
caseCondition = do
    let puppetRegexpCase = do
            reg <- termRegexp
            void $ symbolic ':'
            stmts <- braces statementList
            return [ (Terminal (URegexp reg), stmts) ]
        defaultCase = do
            try (reserved "default")
            void $ symbolic ':'
            stmts <- braces statementList
            return [ (Terminal (UBoolean True), stmts) ]
        puppetCase = do
            compares <- expression `sepBy1` comma
            void $ symbolic ':'
            stmts <- braces statementList
            return $ map (,stmts) compares
        condsToExpression e (x, stmts) = f x :!: stmts
            where f = case x of
                          (Terminal (UBoolean _))-> id
                          (Terminal (URegexp _)) -> RegexMatch e
                          _                      -> Equal e
    p <- getPosition
    reserved "case"
    expr1 <- expression
    condlist <- braces (some (puppetRegexpCase <|> defaultCase <|> puppetCase))
    pe <- getPosition
    return (CondStatement (V.fromList (map (condsToExpression expr1) (concat condlist))) (p :!: pe) )

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
    x <- resourceDeclaration `sepEndBy1` (symbolic ';' <|> comma)
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

resourceCollection :: Position -> T.Text -> Parser RColl
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
    return (RColl collectortype restype e (V.fromList overrides) (p :!: pe) )

classDefinition :: Parser ClassDecl
classDefinition = do
    p <- getPosition
    reserved "class"
    ClassDecl <$> className
              <*> option V.empty puppetClassParameters
              <*> option S.Nothing (fmap S.Just (reserved "inherits" *> className))
              <*> braces statementList
              <*> ( (p :!:) <$> getPosition )

mainFunctionCall :: Parser MFC
mainFunctionCall = do
    p <- getPosition
    (fname, args) <- genFunctionCall True
    pe <- getPosition
    return (MFC fname args (p :!: pe))

mainHFunctionCall :: Parser SFC
mainHFunctionCall = do
    p <- getPosition
    fc <- try hfunctionCall
    pe <- getPosition
    return (SFC fc (p :!: pe))

dotCall :: Parser SFC
dotCall = do
    p <- getPosition
    ex <- expression
    pe <- getPosition
    hf <- case ex of
              FunctionApplication e (Terminal (UHFunctionCall hf)) -> do
                  unless (S.isNothing (hf ^. hfexpr)) (fail "Can't call a function with . and ()")
                  return (hf & hfexpr .~ S.Just e)
              Terminal (UHFunctionCall hf) -> do
                  when (S.isNothing (hf ^. hfexpr)) (fail "This function needs data to operate on")
                  return hf
              _ -> fail "A method chained by dots."
    unless (hf ^. hftype == HFEach) (fail "Expected 'each', the other types of method calls are not supported by language-puppet at the statement level.")
    return (SFC hf (p :!: pe))

data ChainableStuff = ChainResColl RColl
                    | ChainResDecl ResDec
                    | ChainResRefr T.Text [Expression] PPosition

resourceDefaults :: Parser DefaultDec
resourceDefaults = do
    p <- getPosition
    rnd  <- resourceNameRef
    let assignmentList = V.fromList <$> assignment `sepEndBy1` comma
    asl <- braces assignmentList
    pe <- getPosition
    return (DefaultDec rnd asl (p :!: pe))

resourceOverride :: Parser [ResOver]
resourceOverride = do
    p <- getPosition
    restype  <- resourceNameRef
    names <- brackets (expression `sepBy1` comma) <?> "Resource reference values"
    assignments <- V.fromList <$> braces (assignment `sepEndBy` comma)
    pe <- getPosition
    return [ ResOver restype n assignments (p :!: pe) | n <- names ]

extractResRef :: ChainableStuff -> [(T.Text, Expression, PPosition)]
extractResRef (ChainResColl _) = []
extractResRef (ChainResDecl (ResDec rt rn _ _ pp)) = [(rt,rn,pp)]
extractResRef (ChainResRefr rt rns pp) = [(rt,rn,pp) | rn <- rns]

extractChainStatement :: ChainableStuff -> [Statement]
extractChainStatement (ChainResColl r) = [ResourceCollection r]
extractChainStatement (ChainResDecl d) = [ResourceDeclaration d]
extractChainStatement ChainResRefr{} = []

chainableStuff :: Parser [Statement]
chainableStuff = do
    let withresname = do
            p <- getPosition
            restype  <- resourceNameRef
            lookAhead anyChar >>= \x -> case x of
                '[' -> do
                    resnames <- brackets (expression `sepBy1` comma)
                    pe <- getPosition
                    pure (ChainResRefr restype resnames (p :!: pe))
                _ -> ChainResColl <$> resourceCollection p restype
    chain <- parseRelationships $ pure <$> try withresname <|> map ChainResDecl <$> resourceGroup'
    let relations = do
            (g1, g2, lt) <- zipChain chain
            (rt1, rn1, _   :!: pe1) <- concatMap extractResRef g1
            (rt2, rn2, ps2 :!: _  ) <- concatMap extractResRef g2
            return (Dep (rt1 :!: rn1) (rt2 :!: rn2) lt (pe1 :!: ps2))
    return $ map Dependency relations <> (chain ^.. folded . folded . to extractChainStatement . folded)

statement :: Parser [Statement]
statement =
        (pure . SHFunctionCall <$> try dotCall)
    <|> (pure . VariableAssignment <$> variableAssignment)
    <|> (map Node <$> nodeStmt)
    <|> (pure . DefineDeclaration <$> defineStmt)
    <|> (pure . ConditionalStatement <$> unlessCondition)
    <|> (pure . ConditionalStatement <$> ifCondition)
    <|> (pure . ConditionalStatement <$> caseCondition)
    <|> (pure . DefaultDeclaration <$> try resourceDefaults)
    <|> (map ResourceOverride <$> try resourceOverride)
    <|> chainableStuff
    {-
    <|> resourceGroup
    <|> rrGroup
    -}
    <|> (pure . ClassDeclaration <$> classDefinition)
    <|> (pure . SHFunctionCall <$> mainHFunctionCall)
    <|> (pure . MainFunctionCall <$> mainFunctionCall)
    <?> "Statement"

statementList :: Parser (V.Vector Statement)
statementList = fmap (V.fromList . concat) (many statement)

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
                [a]   -> return (BPSingle a)
                [a,b] -> return (BPPair a b)
                _     -> fail "Invalid number of variables between the pipes"

hfunctionCall :: Parser HFunctionCall
hfunctionCall = do
    let toStrict (Just x) = S.Just x
        toStrict Nothing  = S.Nothing
    HFunctionCall <$> parseHFunction
                  <*> fmap (toStrict . join) (optional (parens (optional expression)))
                  <*> parseHParams
                  <*> (symbolic '{' *> fmap (V.fromList . concat) (many (try statement)))
                  <*> fmap toStrict (optional expression) <* symbolic '}'
