{-# LANGUAGE TupleSections #-}

{-| Parse puppet source code from text. -}
module Puppet.Parser (
  -- * Runner
    runPuppetParser
  -- * Parsers
  , Parser
  , puppetParser
  , PuppetParseError
  , prettyParseError
  -- ** exposed to ease testing
  , expression
  , datatype
  -- * Pretty Print
  , module Puppet.Parser.PrettyPrinter
  , module Puppet.Parser.Types
  , module Puppet.Parser.Lens
) where

import           XPrelude.Extra                   hiding (option, try, many, some)
import           XPrelude.PP hiding (braces, comma, brackets, parens, sep)

import qualified Data.Char                        as Char
import qualified Data.List                        as List
import qualified Data.List.NonEmpty               as NE
import qualified Data.Maybe.Strict                as S
import qualified Data.Scientific                  as Scientific
import qualified Data.Text                        as Text
import qualified Data.Vector                      as V
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as Lexer
import           Control.Monad.Combinators.Expr
import qualified Text.Regex.PCRE.ByteString.Utils as Regex

import           Puppet.Language
import           Puppet.Parser.Lens
import           Puppet.Parser.PrettyPrinter
import           Puppet.Parser.Types

type PuppetParseError = ParseError Char Void
type Parser = Parsec Void Text

-- | Build a 'PrettyError' from a 'ParseError' given the text source.
-- The source is used to display the line on which the error occurs.
prettyParseError :: ParseErrorBundle Text Void -> PrettyError
prettyParseError err = PrettyError $ "cannot parse" <+> pretty (errorBundlePretty err)

-- | Run a puppet parser against some 'Text' input.
runPuppetParser :: String -> Text -> Either (ParseErrorBundle Text Void) (Vector Statement)
runPuppetParser = parse puppetParser

-- space consumer
sc :: Parser ()
sc = Lexer.space space1 (Lexer.skipLineComment "#") (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: Text -> Parser ()
symbol = void . Lexer.symbol sc

symbolic :: Char -> Parser ()
symbolic = symbol . Text.singleton

integerOrDouble :: Parser (Either Integer Double)
integerOrDouble = fmap Left hex <|> (either Right Left . Scientific.floatingOrInteger <$> Lexer.scientific)
    where
        hex = string "0x" *> Lexer.hexadecimal

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

comma :: Parser ()
comma = symbol ","

sepComma :: Parser a -> Parser [a]
sepComma p = p `sepEndBy` comma

sepComma1 :: Parser a -> Parser [a]
sepComma1 p = p `sepEndBy1` comma

-- | Parse a collection of puppet 'Statement'.
puppetParser :: Parser (Vector Statement)
puppetParser = optional sc >> statementList

-- | Parse an 'Expression'.
expression :: Parser Expression
expression =
  condExpression
  <|> makeExprParser (lexeme terminal) expressionTable
  <?> "expression"
  where
    condExpression = do
      selectedExpression <- try $ do
          trm <- lexeme terminal
          lookups <- optional indexLookupChain
          symbolic '?'
          return $ maybe trm ($ trm) lookups
      let cas = do
            c <- (SelectorDefault <$ symbol "default") -- default case
                    <|> fmap SelectorType (try datatype)
                    <|> fmap SelectorValue
                          (   fmap UVariableReference variableReference
                          <|> fmap UBoolean puppetBool
                          <|> (UUndef <$ symbol "undef")
                          <|> literalValue
                          <|> fmap UInterpolable interpolableString
                          <|> (URegexp <$> termRegexp)
                          )
            void $ symbol "=>"
            e <- expression
            return (c :!: e)
      cases <- braces (sepComma1 cas)
      return (ConditionalValue selectedExpression (V.fromList cases))

variable :: Parser Expression
variable = Terminal . UVariableReference <$> variableReference

stringLiteral' :: Parser Text
stringLiteral' = char '\'' *> interior <* symbolic '\''
    where
        interior = Text.pack . concat <$> many (some (noneOf ['\'', '\\']) <|> (char '\\' *> fmap escape anySingle))
        escape '\'' = "'"
        escape x    = ['\\',x]

identifier :: Parser (Tokens Text)
identifier = takeWhile1P (Just "identifierpart") identifierPart

identifierPart :: Char -> Bool
identifierPart x = Char.isAsciiLower x || Char.isAsciiUpper x || Char.isDigit x || (x == '_')

identl :: Parser Char -> Parser Char -> Parser Text
identl fstl nxtl = do
  f <- fstl
  nxt <- lexeme $ many nxtl
  return $ Text.pack $ f : nxt

operator :: Text -> Parser ()
operator = void . try . symbol

reserved :: Text -> Parser ()
reserved s =
  try $ do
    void (string s)
    notFollowedBy (satisfy identifierPart)
    sc

variableName :: Parser Text
variableName = do
    out <- qualif identifier
    when (out == "string") (panic "The special variable $string should never be used")
    return out

qualif :: Parser Text -> Parser Text
qualif p = lexeme $ do
    header <- option "" (string "::")
    ( header <> ) . Text.intercalate "::" <$> p `sepBy1` string "::"

qualif1 :: Parser Text -> Parser Text
qualif1 p = try $ do
    r <- qualif p
    unless ("::" `Text.isInfixOf` r) (fail "This parser is not qualified")
    return r

className :: Parser Text
className = qualif moduleName

-- yay with reserved words
typeName :: Parser Text
typeName = className

moduleName :: Parser Text
moduleName = genericModuleName False

resourceNameRef :: Parser Text
resourceNameRef = qualif (genericModuleName True)

genericModuleName :: Bool -> Parser Text
genericModuleName isReference = do
    let acceptable x = Char.isAsciiLower x || Char.isDigit x || (x == '_')
        firstletter = if isReference
                          then fmap Char.toLower (satisfy Char.isAsciiUpper)
                          else satisfy Char.isAsciiLower
    identl firstletter (satisfy acceptable)

parameterName :: Parser Text
parameterName = moduleName

variableReference :: Parser Text
variableReference = char '$' *> variableName

interpolableString :: Parser (Vector Expression)
interpolableString = V.fromList <$> between (char '"') (symbolic '"')
     ( many (interpolableVariableReference <|> doubleQuotedStringContent <|> fmap (Terminal . UString . Text.singleton) (char '$')) )
    where
        doubleQuotedStringContent = Terminal . UString . Text.pack . concat <$>
            some ((char '\\' *> fmap escaper anySingle) <|> some (noneOf [ '"', '\\', '$' ]))
        escaper :: Char -> String
        escaper 'n'  = "\n"
        escaper 't'  = "\t"
        escaper 'r'  = "\r"
        escaper '"'  = "\""
        escaper '\\' = "\\"
        escaper '$'  = "$"
        escaper x    = ['\\',x]
        -- this is specialized because we can't be "tokenized" here
        variableAccept x = Char.isAsciiLower x || Char.isAsciiUpper x || Char.isDigit x || x == '_'
        rvariableName = do
            v <- Text.concat <$> some (string "::" <|> fmap Text.pack (some (satisfy variableAccept)))
            when (v == "string") (fail "The special variable $string must not be used")
            return v
        rvariable = Terminal . UVariableReference <$> rvariableName
        simpleIndexing = Lookup <$> rvariable <*> between (symbolic '[') (symbolic ']') expression
        interpolableVariableReference = do
            void (char '$')
            let fenced =    try (simpleIndexing <* char '}')
                        <|> try (rvariable <* char '}')
                        <|> (expression <* char '}')
            (symbolic '{' *> fenced) <|> try rvariable <|> pure (Terminal (UString (Text.singleton '$')))

regexp :: Parser Text
regexp = do
    void (char '/')
    Text.pack . concat <$> many ( do { void (char '\\') ; x <- anySingle; return ['\\', x] } <|> some (noneOf [ '/', '\\' ]) )
        <* symbolic '/'

puppetArray :: Parser UnresolvedValue
puppetArray = fmap (UArray . V.fromList) (brackets (sepComma expression)) <?> "Array"

puppetHash :: Parser UnresolvedValue
puppetHash = fmap (UHash . V.fromList) (braces (sepComma hashPart)) <?> "Hash"
    where
        hashPart = (:!:) <$> (expression <* operator "=>")
                         <*> expression

puppetBool :: Parser Bool
puppetBool =  (reserved "true" >> return True)
          <|> (reserved "false" >> return False)
          <?> "Boolean"

resourceReferenceRaw :: Parser (Text, [Expression])
resourceReferenceRaw = do
    restype  <- resourceNameRef <?> "Resource reference type"
    resnames <- brackets (expression `sepBy1` comma) <?> "Resource reference values"
    return (restype, resnames)

resourceReference :: Parser UnresolvedValue
resourceReference = do
    (restype, resnames) <- resourceReferenceRaw
    return $ UResourceReference restype $ case resnames of
                 [x] -> x
                 _   -> Terminal $ UArray (V.fromList resnames)

bareword :: Parser Text
bareword = identl (satisfy Char.isAsciiLower) (satisfy acceptable) <?> "Bare word"
    where
        acceptable x = Char.isAsciiLower x || Char.isAsciiUpper x || Char.isDigit x || (x == '_') || (x == '-')

-- The first argument defines if non-parenthesized arguments are acceptable
genFunctionCall :: Bool -> Parser (Text, Vector Expression)
genFunctionCall nonparens = do
    fname <- moduleName <?> "Function name"
    -- this is a hack. Contrary to what the documentation says,
    -- a "bareword" can perfectly be a qualified name :
    -- include foo::bar
    let argsc sep e = (fmap (Terminal . UString) (qualif1 className) <|> e <?> "Function argument A") `sep` comma
        terminalF = terminalG (fail "function hack")
        expressionF = makeExprParser (lexeme terminalF) expressionTable <?> "function expression"
        withparens = parens (argsc sepEndBy expression)
        withoutparens = argsc sepEndBy1 expressionF
    args  <- withparens <|> if nonparens
                                then withoutparens <?> "Function arguments B"
                                else fail "Function arguments C"
    return (fname, V.fromList args)


literalValue :: Parser UnresolvedValue
literalValue = lexeme (fmap UString stringLiteral' <|> fmap UString bareword <|> fmap UNumber numericalvalue <?> "Literal Value")
    where
        numericalvalue = integerOrDouble >>= \i -> case i of
            Left x  -> return (fromIntegral x)
            Right y -> return (Scientific.fromFloatDigits y)

-- this is a hack for functions :(
terminalG :: Parser Expression -> Parser Expression
terminalG g = parens expression
         <|> fmap (Terminal . UInterpolable) interpolableString
         <|> (Terminal UUndef <$ reserved "undef")
         <|> fmap (Terminal . URegexp) termRegexp
         <|> variable
         <|> fmap Terminal puppetArray
         <|> fmap Terminal puppetHash
         <|> fmap (Terminal . UBoolean) puppetBool
         <|> fmap (Terminal . UDataType) datatype
         <|> fmap Terminal resourceReference
         <|> g
         <|> fmap Terminal literalValue

compileRegexp :: Text -> Parser CompRegex
compileRegexp p = case Regex.compile' Regex.compBlank Regex.execBlank (encodeUtf8 p) of
    Right r -> return $ CompRegex p r
    Left ms -> fail ("Can't parse regexp /" ++ Text.unpack p ++ "/ : " ++ show ms)

termRegexp :: Parser CompRegex
termRegexp = regexp >>= compileRegexp

terminal :: Parser Expression
terminal = terminalG (fmap Terminal (fmap UHOLambdaCall (try lambdaCall) <|> try funcCall))
    where
        funcCall :: Parser UnresolvedValue
        funcCall = do
            (fname, args) <- genFunctionCall False
            return $ UFunctionCall fname args

expressionTable :: [[Operator Parser Expression]]
expressionTable = [ [ Postfix indexLookupChain ] -- http://stackoverflow.com/questions/10475337/parsec-expr-repeated-prefix-postfix-operator-not-supported
                  , [ Prefix ( operator "-"   >> return Negate           ) ]
                  , [ Prefix ( operator "!"   >> return Not              ) ]
                  , [ InfixL  ( operator "."   >> return FunctionApplication ) ]
                  , [ InfixL  ( reserved "in"  >> return Contains         ) ]
                  , [ InfixL  ( operator "/"   >> return Division         )
                    , InfixL  ( operator "*"   >> return Multiplication   )
                    ]
                  , [ InfixL  ( operator "+"   >> return Addition     )
                    , InfixL  ( operator "-"   >> return Substraction )
                    ]
                  , [ InfixL  ( operator "<<"  >> return LeftShift  )
                    , InfixL  ( operator ">>"  >> return RightShift )
                    ]
                  , [ InfixL  ( operator "=="  >> return Equal     )
                    , InfixL  ( operator "!="  >> return Different )
                    ]
                  , [ InfixL  ( operator "=~"  >> return RegexMatch    )
                    , InfixL  ( operator "!~"  >> return NotRegexMatch )
                    ]
                  , [ InfixL  ( operator ">="  >> return MoreEqualThan )
                    , InfixL  ( operator "<="  >> return LessEqualThan )
                    , InfixL  ( operator ">"   >> return MoreThan      )
                    , InfixL  ( operator "<"   >> return LessThan      )
                    ]
                  , [ InfixL  ( reserved "and" >> return And )
                    , InfixL  ( reserved "or"  >> return Or  )
                    ]
                  ]

indexLookupChain :: Parser (Expression -> Expression)
indexLookupChain = List.foldr1 (flip (.)) <$> some checkLookup
    where
        checkLookup = flip Lookup <$> between (operator "[") (operator "]") expression

stringExpression :: Parser Expression
stringExpression = fmap (Terminal . UInterpolable) interpolableString <|> (reserved "undef" *> return (Terminal UUndef)) <|> fmap (Terminal . UBoolean) puppetBool <|> variable <|> fmap Terminal literalValue

varAssign :: Parser VarAssignDecl
varAssign = do
    p <- getSourcePos
    mt <- optional datatype
    v <- variableReference
    void $ symbolic '='
    e <- expression
    when (Text.all Char.isDigit v) (fail "Can't assign fully numeric variables")
    pe <- getSourcePos
    return (VarAssignDecl mt v e (p :!: pe))

nodeDecl :: Parser [NodeDecl]
nodeDecl = do
    p <- getSourcePos
    reserved "node"
    let toString (UString s) = s
        toString (UNumber n) = scientific2text n
        toString _           = panic "Can't happen at nodeDecl"
        nodename = (reserved "default" >> return NodeDefault) <|> fmap (NodeName . toString) literalValue
    ns <- (fmap NodeMatch termRegexp <|> nodename) `sepBy1` comma
    inheritance <- option S.Nothing (fmap S.Just (reserved "inherits" *> nodename))
    st <- braces statementList
    pe <- getSourcePos
    return [NodeDecl n st inheritance (p :!: pe) | n <- ns]

defineDecl :: Parser DefineDecl
defineDecl = do
    p <- getSourcePos
    reserved "define"
    name <- typeName
    -- TODO check native type
    params <- option V.empty puppetClassParameters
    st <- braces statementList
    pe <- getSourcePos
    return (DefineDecl name params st (p :!: pe))

puppetClassParameters :: Parser (Vector (Pair (Pair Text (S.Maybe UDataType)) (S.Maybe Expression)))
puppetClassParameters = V.fromList <$> parens (sepComma var)
    where
        toStrictMaybe (Just x) = S.Just x
        toStrictMaybe Nothing  = S.Nothing
        var :: Parser (Pair (Pair Text (S.Maybe UDataType)) (S.Maybe Expression))
        var = do
          tp <- toStrictMaybe <$> optional datatype
          n  <- variableReference
          df <- toStrictMaybe <$> optional (symbolic '=' *> expression)
          return (n :!: tp :!: df)

puppetIfStyleCondition :: Parser (Pair Expression (Vector Statement))
puppetIfStyleCondition = (:!:) <$> expression <*> braces statementList

unlessCondition :: Parser ConditionalDecl
unlessCondition = do
    p <- getSourcePos
    reserved "unless"
    (cond :!: stmts) <- puppetIfStyleCondition
    pe <- getSourcePos
    return (ConditionalDecl (V.singleton (Not cond :!: stmts)) (p :!: pe))

ifCondition :: Parser ConditionalDecl
ifCondition = do
    p <- getSourcePos
    reserved "if"
    maincond <- puppetIfStyleCondition
    others   <- many (reserved "elsif" *> puppetIfStyleCondition)
    elsecond <- option V.empty (reserved "else" *> braces statementList)
    let ec = if V.null elsecond
                 then []
                 else [Terminal (UBoolean True) :!: elsecond]
    pe <- getSourcePos
    return (ConditionalDecl (V.fromList (maincond : others ++ ec)) (p :!: pe))

caseCondition :: Parser ConditionalDecl
caseCondition = do
    let puppetRegexpCase = Terminal . URegexp <$> termRegexp
        defaultCase = Terminal (UBoolean True) <$ try (reserved "default")
        matchesToExpression e (x, stmts) = f x :!: stmts
            where f = case x of
                          (Terminal (UBoolean _)) -> identity
                          (Terminal (URegexp _))  -> RegexMatch e
                          _                       -> Equal e
        cases = do
            matches <- (puppetRegexpCase <|> defaultCase <|> expression) `sepBy1` comma
            void $ symbolic ':'
            stmts <- braces statementList
            return $ map (,stmts) matches
    p <- getSourcePos
    reserved "case"
    expr1 <- expression
    condlist <- concat <$> braces (some cases)
    pe <- getSourcePos
    return (ConditionalDecl (V.fromList (map (matchesToExpression expr1) condlist)) (p :!: pe) )

data OperatorChain a = OperatorChain a LinkType (OperatorChain a)
                     | EndOfChain a

instance Foldable OperatorChain where
    foldMap f (EndOfChain x)         = f x
    foldMap f (OperatorChain a _ nx) = f a <> foldMap f nx

operatorChainStatement :: OperatorChain a -> a
operatorChainStatement (OperatorChain a _ _) = a
operatorChainStatement (EndOfChain x)        = x

zipChain :: OperatorChain a -> [ ( a, a, LinkType ) ]
zipChain (OperatorChain a d nx) = (a, operatorChainStatement nx, d) : zipChain nx
zipChain (EndOfChain _) = []

depOperator :: Parser LinkType
depOperator =   (RBefore <$ operator "->")
            <|> (RNotify <$ operator "~>")

assignment :: Parser AttributeDecl
assignment = (AttributeDecl <$> key <*> arrowOp  <*> expression)
         <|> (AttributeWildcard <$> (symbolic '*' *> symbol "=>" *> expression))
    where
        key = identl (satisfy Char.isAsciiLower) (satisfy acceptable) <?> "Assignment key"
        acceptable x = Char.isAsciiLower x || Char.isAsciiUpper x || Char.isDigit x || (x == '_') || (x == '-')
        arrowOp =
              (AssignArrow <$ symbol "=>")
          <|> (AppendArrow <$ symbol "+>")

searchExpression :: Parser SearchExpression
searchExpression = makeExprParser (lexeme searchterm) searchTable
    where
        searchTable :: [[Operator Parser SearchExpression]]
        searchTable = [ [ InfixL ( reserved "and"   >> return AndSearch )
                        , InfixL ( reserved "or"    >> return OrSearch  )
                        ] ]
        searchterm = parens searchExpression <|> check
        check = do
            attrib <- parameterName
            opr    <- (EqualitySearch <$ operator "==")
                  <|> (NonEqualitySearch <$ operator "!=")
            term   <- stringExpression
            return (opr attrib term)

resCollDecl :: Position -> Text -> Parser ResCollDecl
resCollDecl p restype = do
    openchev <- some (char '<')
    when (length openchev > 2) (fail "Too many brackets")
    void $ symbolic '|'
    e <- option AlwaysTrue searchExpression
    void (char '|')
    void (count (length openchev) (char '>'))
    sc
    overrides <- option [] $ braces (sepComma assignment)
    let collectortype = if length openchev == 1
                            then Collector
                            else ExportedCollector
    pe <- getSourcePos
    return (ResCollDecl collectortype restype e (V.fromList overrides) (p :!: pe) )

classDecl :: Parser ClassDecl
classDecl = do
    p <- getSourcePos
    reserved "class"
    ClassDecl <$> className
              <*> option V.empty puppetClassParameters
              <*> option S.Nothing (fmap S.Just (reserved "inherits" *> className))
              <*> braces statementList
              <*> ( (p :!:) <$> getSourcePos )

mainFuncDecl :: Parser MainFuncDecl
mainFuncDecl = do
    p <- getSourcePos
    (fname, args) <- genFunctionCall True
    pe <- getSourcePos
    return (MainFuncDecl fname args (p :!: pe))

hoLambdaDecl :: Parser HigherOrderLambdaDecl
hoLambdaDecl = do
    p <- getSourcePos
    fc <- try lambdaCall
    pe <- getSourcePos
    return (HigherOrderLambdaDecl fc (p :!: pe))

dotLambdaDecl :: Parser HigherOrderLambdaDecl
dotLambdaDecl = do
    p <- getSourcePos
    ex <- expression
    pe <- getSourcePos
    hf <- case ex of
              FunctionApplication e (Terminal (UHOLambdaCall hf)) -> do
                  unless (null (hf ^. hoLambdaExpr)) (fail "Can't call a function with . and ()")
                  return (hf & hoLambdaExpr .~ V.singleton e)
              Terminal (UHOLambdaCall hf) -> do
                  when (null (hf ^. hoLambdaExpr)) (fail "This function needs data to operate on")
                  return hf
              _ -> fail "A method chained by dots."
    return (HigherOrderLambdaDecl hf (p :!: pe))


resDefaultDecl :: Parser ResDefaultDecl
resDefaultDecl = do
    p <- getSourcePos
    rnd  <- resourceNameRef
    let assignmentList = V.fromList <$> sepComma1 assignment
    asl <- braces assignmentList
    pe <- getSourcePos
    return (ResDefaultDecl rnd asl (p :!: pe))

resOverrideDecl :: Parser [ResOverrideDecl]
resOverrideDecl = do
    p <- getSourcePos
    restype  <- resourceNameRef
    names <- brackets (expression `sepBy1` comma) <?> "Resource reference values"
    assignments <- V.fromList <$> braces (sepComma assignment)
    pe <- getSourcePos
    return [ ResOverrideDecl restype n assignments (p :!: pe) | n <- names ]

-- | Heterogeneous chain (interleaving resource declarations with
-- resource references) needs to be supported:
--
--    class { 'docker::service': } ->
--    Class['docker']
chainableResources :: Parser [Statement]
chainableResources = do
    let withresname = do
            p <- getSourcePos
            restype  <- resourceNameRef
            lookAhead anySingle >>= \x -> case x of
                '[' -> do
                    resnames <- brackets (expression `sepBy1` comma)
                    pe <- getSourcePos
                    pure (ChainResRefr restype resnames (p :!: pe))
                _ -> ChainResColl <$> resCollDecl p restype
    chain <- parseRelationships $ pure <$> try withresname <|> map ChainResDecl <$> resDeclGroup
    let relations = do
            (g1, g2, lt) <- zipChain chain
            (rt1, rn1, _   :!: pe1) <- concatMap extractResRef g1
            (rt2, rn2, ps2 :!: _  ) <- concatMap extractResRef g2
            return (DepDecl (rt1 :!: rn1) (rt2 :!: rn2) lt (pe1 :!: ps2))
    return $ map DependencyDeclaration relations <> (chain ^.. folded . folded . to extractChainStatement . folded)
  where
    extractResRef :: ChainableRes -> [(Text, Expression, PPosition)]
    extractResRef (ChainResColl _) = []
    extractResRef (ChainResDecl (ResDecl rt rn _ _ pp)) = [(rt,rn,pp)]
    extractResRef (ChainResRefr rt rns pp) = [(rt,rn,pp) | rn <- rns]

    extractChainStatement :: ChainableRes -> [Statement]
    extractChainStatement (ChainResColl r) = [ResourceCollectionDeclaration r]
    extractChainStatement (ChainResDecl d) = [ResourceDeclaration d]
    extractChainStatement ChainResRefr{}   = []

    parseRelationships :: Parser a -> Parser (OperatorChain a)
    parseRelationships p = do
        g <- p
        o <- optional depOperator
        case o of
            Just o' -> OperatorChain g o' <$> parseRelationships p
            Nothing -> pure (EndOfChain g)

    resDeclGroup :: Parser [ResDecl]
    resDeclGroup = do
        let resourceName = expression
            resourceDeclaration = do
                p <- getSourcePos
                names <- brackets (sepComma1 resourceName) <|> fmap return resourceName
                void $ symbolic ':'
                vals  <- fmap V.fromList (sepComma assignment)
                pe <- getSourcePos
                return [(n, vals, p :!: pe) | n <- names ]
            groupDeclaration = (,) <$> many (char '@') <*> typeName <* symbolic '{'
        (virts, rtype) <- try groupDeclaration -- for matching reasons, this gets a try until the opening brace
        let sep = symbolic ';' <|> comma
        x <- resourceDeclaration `sepEndBy1` sep
        void $ symbolic '}'
        virtuality <- case virts of
                          ""   -> return Normal
                          "@"  -> return Virtual
                          "@@" -> return Exported
                          _    -> fail "Invalid virtuality"
        return [ ResDecl rtype rname conts virtuality pos | (rname, conts, pos) <- concat x ]

statement :: Parser [Statement]
statement =
        (pure . HigherOrderLambdaDeclaration <$> try dotLambdaDecl)
    <|> (pure . VarAssignmentDeclaration <$> varAssign)
    <|> (map NodeDeclaration <$> nodeDecl)
    <|> (pure . DefineDeclaration <$> defineDecl)
    <|> (pure . ConditionalDeclaration <$> unlessCondition)
    <|> (pure . ConditionalDeclaration <$> ifCondition)
    <|> (pure . ConditionalDeclaration <$> caseCondition)
    <|> (pure . ResourceDefaultDeclaration <$> try resDefaultDecl)
    <|> (map ResourceOverrideDeclaration <$> try resOverrideDecl)
    <|> chainableResources
    <|> (pure . ClassDeclaration <$> classDecl)
    <|> (pure . HigherOrderLambdaDeclaration <$> hoLambdaDecl)
    <|> (pure . MainFunctionDeclaration <$> mainFuncDecl)
    <?> "Statement"

datatype :: Parser UDataType
datatype = dtString
       <|> dtInteger
       <|> dtFloat
       <|> dtNumeric
       <|> (UDTBoolean <$ reserved "Boolean")
       <|> (UDTScalar <$ reserved "Scalar")
       <|> (UDTData <$ reserved "Data")
       <|> (UDTAny <$ reserved "Any")
       <|> (UDTCollection <$ reserved "Collection")
       <|> dtArray
       <|> dtHash
       <|> (UDTUndef <$ reserved "Undef")
       <|> (reserved "Optional" *> (UDTOptional <$> brackets datatype))
       <|> (UNotUndef <$ reserved "NotUndef")
       <|> (reserved "Variant" *> (UDTVariant . NE.fromList <$> brackets (datatype `sepBy1` symbolic ',')))
       <|> (reserved "Regexp" *> (UDTRegexp <$> optional (brackets termRegexp)))
       -- while all the other cases are straightforward, it seems that the
       -- following syntax is a valid regexp for puppet:
       --   '^dqsqsdqs$'
       -- instead of:
       --   /^dqsqsdqs$/
       --
       -- That is the reason there is a "quotedRegexp" case
       <|> (reserved "Pattern" *> (UDTPattern . NE.fromList <$> brackets ( (termRegexp <|> quotedRegexp) `sepBy1` symbolic ',')))
       <|> (reserved "Enum" *> (UDTEnum . NE.fromList <$> brackets (expression `sepBy1` symbolic ',')))
       <|> dtExternal
       <?> "UDataType"
  where
    quotedRegexp = stringLiteral' >>= compileRegexp
    integer = integerOrDouble >>= either (return . fromIntegral) (\d -> fail ("Integer value expected, instead of " ++ show d))
    float = either fromIntegral identity <$> integerOrDouble
    dtArgs str def parseArgs = do
      void $ reserved str
      fromMaybe def <$> optional (brackets parseArgs)
    dtbounded s constructor parser = dtArgs s (constructor Nothing Nothing) $ do
      lst <- parser `sepBy1` symbolic ','
      case lst of
        [minlen] -> return $ constructor (Just minlen) Nothing
        [minlen,maxlen] -> return $ constructor (Just minlen) (Just maxlen)
        _ -> fail ("Too many arguments to datatype " ++ Text.unpack s)
    dtString = dtbounded "String" UDTString integer
    dtInteger = dtbounded "Integer" UDTInteger integer
    dtFloat = dtbounded "Float" UDTFloat float
    dtNumeric = dtbounded "Numeric" (\ma mb -> UDTVariant (UDTFloat ma mb :| [UDTInteger (truncate <$> ma) (truncate <$> mb)])) float
    dtArray = do
      reserved "Array"
      ml <- optional $ brackets $ do
        tp <- datatype
        rst <- optional (symbolic ',' *> integer `sepBy1` symbolic ',')
        return (tp, rst)
      case ml of
        Nothing -> return (UDTArray UDTData 0 Nothing)
        Just (t, Nothing) -> return (UDTArray t 0 Nothing)
        Just (t, Just [mi]) -> return (UDTArray t mi Nothing)
        Just (t, Just [mi, mx]) -> return (UDTArray t mi (Just mx))
        Just (_, Just _) -> fail "Too many arguments to datatype Array"
    dtHash = do
      reserved "Hash"
      ml <- optional $ brackets $ do
        tk <- datatype
        symbolic ','
        tv <- datatype
        rst <- optional (symbolic ',' *> integer `sepBy1` symbolic ',')
        return (tk, tv, rst)
      case ml of
        Nothing -> return (UDTHash UDTScalar UDTData 0 Nothing)
        Just (tk, tv, Nothing) -> return (UDTHash tk tv 0 Nothing)
        Just (tk, tv, Just [mi]) -> return (UDTHash tk tv mi Nothing)
        Just (tk, tv, Just [mi, mx]) -> return (UDTHash tk tv mi (Just mx))
        Just (_, _, Just _) -> fail "Too many arguments to datatype Hash"
    dtExternal =
      reserved "Stdlib::Absolutepath" $> UDTData
      <|> reserved "Stdlib::Base32" $> UDTData
      <|> reserved "Stdlib::Base64" $> UDTData
      <|> reserved "Stdlib::Compat::Absolute_path" $> UDTData
      <|> reserved "Stdlib::Compat::Array" $> UDTData
      <|> reserved "Stdlib::Compat::Bool" $> UDTData
      <|> reserved "Stdlib::Compat::Float" $> UDTData
      <|> reserved "Stdlib::Compat::Hash" $> UDTData
      <|> reserved "Stdlib::Compat::Integer" $> UDTData
      <|> reserved "Stdlib::Compat::Ip_address" $> UDTData
      <|> reserved "Stdlib::Compat::Ipv4" $> UDTData
      <|> reserved "Stdlib::Compat::Ipv6" $> UDTData
      <|> reserved "Stdlib::Compat::Numeric" $> UDTData
      <|> reserved "Stdlib::Compat::String" $> UDTData
      <|> reserved "Stdlib::Ensure::Service" $> UDTData
      <|> reserved "Stdlib::Filemode" $> UDTData
      <|> reserved "Stdlib::Filesource" $> UDTData
      <|> reserved "Stdlib::Fqdn" $> UDTData
      <|> reserved "Stdlib::Host" $> UDTData
      <|> reserved "Stdlib::HTTPSUrl" $> UDTData
      <|> reserved "Stdlib::HTTPUrl" $> UDTData
      <|> reserved "Stdlib::IP::Address::Nosubnet" $> UDTData
      <|> reserved "Stdlib::Ip_address" $> UDTData
      <|> reserved "Stdlib::IP::Address" $> UDTData
      <|> reserved "Stdlib::IP::Address::V4::CIDR" $> UDTData
      <|> reserved "Stdlib::IP::Address::V4::Nosubnet" $> UDTData
      <|> reserved "Stdlib::IP::Address::V4" $> UDTData
      <|> reserved "Stdlib::IP::Address::V6::Alternative" $> UDTData
      <|> reserved "Stdlib::IP::Address::V6::Compressed" $> UDTData
      <|> reserved "Stdlib::IP::Address::V6::Full" $> UDTData
      <|> reserved "Stdlib::IP::Address::V6::Nosubnet::Alternative" $> UDTData
      <|> reserved "Stdlib::IP::Address::V6::Nosubnet::Compressed" $> UDTData
      <|> reserved "Stdlib::IP::Address::V6::Nosubnet::Full" $> UDTData
      <|> reserved "Stdlib::IP::Address::V6::Nosubnet" $> UDTData
      <|> reserved "Stdlib::IP::Address::V6" $> UDTData
      <|> reserved "Stdlib::Ipv4" $> UDTData
      <|> reserved "Stdlib::Ipv6" $> UDTData
      <|> reserved "Stdlib::MAC" $> UDTData
      <|> reserved "Stdlib::Port::Privileged" $> UDTData
      <|> reserved "Stdlib::Port" $> UDTData
      <|> reserved "Stdlib::Port::Unprivileged" $> UDTData
      <|> reserved "Stdlib::Unixpath" $> UDTData
      <|> reserved "Stdlib::Windowspath" $> UDTData
      <|> reserved "Nginx::ErrorLogSeverity" $> UDTData
      <|> reserved "Jenkins::Tunnel" $> UDTData
      <|> reserved "Systemd::Unit" $> UDTData
      <|> reserved "Systemd::ServiceLimits" $> UDTData
      <|> reserved "Systemd::Dropin" $> UDTData

statementList :: Parser (Vector Statement)
statementList = (V.fromList . concat) <$> many statement

lambdaCall :: Parser HOLambdaCall
lambdaCall = do
    let tostrict (Just x) = S.Just x
        tostrict Nothing  = S.Nothing
    HOLambdaCall <$> lambFunc
                 <*> parameters
                 <*> lambParams
                 <*> (symbolic '{' *> fmap (V.fromList . concat) (many (try statement)))
                 <*> fmap tostrict (optional expression) <* symbolic '}'
    where
      parameters :: Parser (V.Vector Expression)
      parameters = maybe V.empty V.fromList <$> optional (parens (expression `sepBy` comma))
      lambFunc :: Parser LambdaFunc
      lambFunc = LambdaFunc <$> moduleName
      lambParams :: Parser LambdaParameters
      lambParams = between (symbolic '|') (symbolic '|') hp
        where
          lambdaParameter :: Parser LambdaParameter
          lambdaParameter = LParam <$> optional datatype <*> (char '$' *> identifier)
          hp = V.fromList <$> lambdaParameter `sepBy1` comma
