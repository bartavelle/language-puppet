{-# LANGUAGE TupleSections #-}
module Puppet.Parser.Internal
where

import           XPrelude.Extra                   hiding (many, option, some, try)

import           Control.Monad.Combinators.Expr
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
import qualified Text.Regex.PCRE.ByteString.Utils as Regex

import           Puppet.Language
import           Puppet.Parser.Types

-- | Space consumer
sc :: Parser ()
sc = Lexer.space space1 (Lexer.skipLineComment "#") (Lexer.skipBlockComment "/*" "*/")

-- | Lexeme consumes spaces after the input parser
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

-- | Consumes a text then consumes spaces
symbol :: Text -> Parser ()
symbol = void . Lexer.symbol sc

-- | Consumes a character then consumes spaces
symbolic :: Char -> Parser ()
symbolic = lexeme . void . single

braces :: Parser a -> Parser a
braces = between (symbolic '{') (symbolic '}')

parens :: Parser a -> Parser a
parens = between (symbolic '(') (symbolic ')')

brackets :: Parser a -> Parser a
brackets = between (symbolic '[') (symbolic ']')

comma :: Parser ()
comma = symbolic ','

sepComma :: Parser a -> Parser [a]
sepComma p = p `sepEndBy` comma

sepComma1 :: Parser a -> Parser [a]
sepComma1 p = p `sepEndBy1` comma

-- | Parses an 'Expression'.
expression :: Parser Expression
expression = do
  expr <- makeExprParser (lexeme terminal) expressionTable
  ms <- optional $ do
    symbolic '?'
    let case_expr = do
          c <-     SelectorDefault <$ symbol "default" -- default case
               <|> SelectorType <$> try datatype
               <|> fmap SelectorValue
                     (   UVariableReference <$> variableReference
                     <|> UBoolean <$> puppetBool
                     <|> UUndef <$ symbol "undef"
                     <|> literalValue
                     <|> UInterpolable <$> interpolableString
                     <|> URegexp <$> termRegexp
                     )
          symbol "=>"
          e <- expression
          pure (c :!: e)
    cases <- braces (sepComma1 case_expr)
    pure (ConditionalValue expr (V.fromList cases))
  case ms of
    Nothing -> pure expr
    Just cv -> pure cv

stringLiteral' :: Parser Text
stringLiteral' = between (char '\'') (symbolic '\'') interior
  where
    interior = Text.pack . concat <$> many (some (noneOf ['\'', '\\']) <|> (char '\\' *> fmap escape anySingle))
    escape '\'' = "'"
    escape x    = ['\\',x]

identifier :: Parser (Tokens Text)
identifier = takeWhile1P Nothing isIdentifierChar

-- | Only Ascii, hyphens (-) are not allowed.
isIdentifierChar :: Char -> Bool
isIdentifierChar x = Char.isAsciiLower x || Char.isAsciiUpper x || Char.isDigit x || (x == '_')

-- | Like 'indentifier' but hyphens (-) are allowed.
bareword :: Parser Text
bareword = Text.cons <$> satisfy Char.isAsciiLower <*> takeWhileP Nothing isBarewordChar
  where
    isBarewordChar :: Char -> Bool
    isBarewordChar x = isIdentifierChar x || (x == '-')

reserved :: Text -> Parser ()
reserved s =
  try $ do
    void (chunk s)
    notFollowedBy (satisfy isIdentifierChar)
    sc

qualif :: Parser Text -> Parser Text
qualif p = do
  header <- option "" (chunk "::")
  ( header <> ) . Text.intercalate "::" <$> p `sepBy1` chunk "::"

qualif1 :: Parser Text -> Parser Text
qualif1 p = do
  r <- qualif p
  unless ("::" `Text.isInfixOf` r) (fail "This parser is not qualified")
  pure r

-- | Consumes a var $foo and then spaces
variableReference :: Parser Text
variableReference = do
  v <- char '$' *> lexeme variableName
  when (Text.all Char.isDigit v) (fail "Can't assign fully numeric variables")
  pure v

variableName :: Parser Text
variableName = qualif identifier

-- yay with reserved words
typeName :: Parser Text
typeName = className

className :: Parser Text
className = lexeme $ qualif $ genericModuleName False

funcName :: Parser Text
funcName = lexeme $ qualif $ genericModuleName False

moduleName :: Parser Text
moduleName = lexeme $ genericModuleName False

parameterName :: Parser Text
parameterName = moduleName

resourceNameRef :: Parser Text
resourceNameRef = lexeme $ qualif (genericModuleName True)

genericModuleName :: Bool -> Parser Text
genericModuleName isReference = do
  let acceptable x = Char.isAsciiLower x || Char.isDigit x || (x == '_')
      firstletter = if isReference
                      then fmap Char.toLower (satisfy Char.isAsciiUpper)
                      else satisfy Char.isAsciiLower
  (Text.cons) <$> firstletter <*> takeWhileP Nothing acceptable

-- | Variable expression
varExpression :: Parser Expression
varExpression = Terminal . UVariableReference <$> variableReference

-- | String interpolation
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
    varname = Text.concat <$> some (chunk "::" <|> identifier)
    varexpr = Terminal . UVariableReference <$> varname
    indexchain =  makeExprParser varexpr [[Postfix indexLookupChain]] -- e.g: os['release']['major']
    interpolableVariableReference = do
      void (char '$')
      let fenced =    try (indexchain <* char '}')
                  <|> try (varexpr <* char '}')
                  <|> (expression <* char '}')
      (symbolic '{' *> fenced) <|> try varexpr <|> pure (Terminal (UString (Text.singleton '$')))

integerOrDouble :: Parser (Either Integer Double)
integerOrDouble = Left <$> hex <|> (either Right Left . Scientific.floatingOrInteger <$> Lexer.scientific)
  where
    hex = chunk "0x" *> Lexer.hexadecimal

puppetArray :: Parser UnresolvedValue
puppetArray = fmap (UArray . V.fromList) (brackets (sepComma expression)) <?> "Array"

puppetHash :: Parser UnresolvedValue
puppetHash = fmap (UHash . V.fromList) (braces (sepComma hashPart)) <?> "Hash"
  where
    hashPart = (:!:) <$> (expression <* symbol "=>") <*> expression

puppetBool :: Parser Bool
puppetBool =
      (reserved "true" >> pure True)
  <|> (reserved "false" >> pure False)
  <?> "Boolean"

resourceReferenceRaw :: Parser (Text, [Expression])
resourceReferenceRaw = do
  let restype_parser = qualif (genericModuleName True)
      resnames_parser = brackets (expression `sepBy1` comma)
  (,) <$> restype_parser <*> resnames_parser <?> "Resource reference"

resourceReference :: Parser UnresolvedValue
resourceReference = do
  (restype, resnames) <- resourceReferenceRaw
  pure $ UResourceReference restype $ case resnames of
           [x] -> x
           _   -> Terminal $ UArray (V.fromList resnames)

-- | Functions that have named that are not valid ...
specialFunctions :: Parser Text
specialFunctions =
      chunk "Integer"
  <|> chunk "Numeric"

-- The first argument defines if non-parenthesized arguments are acceptable
genFunctionCall :: Bool -> Parser (Text, Vector Expression)
genFunctionCall nonparens = do
  fname <- (specialFunctions <|> funcName) <?> "Function name"
  let
      -- first check if the function arg is not a qualified name (ex.: include foo::bar)
      -- if it is not, then we expect an expression
      qualif_param = (Terminal . UString) <$> qualif1 moduleName <* notFollowedBy (single '(') -- <* lookAhead (anySingleBut '(')
      func_arg expr =  try qualif_param <|> expr <?> "Function argument"
      terminalF = terminalG FunctionWithoutParens
      expressionF = makeExprParser (lexeme terminalF) expressionTable <?> "Function expression"
      withparens = parens (func_arg expression `sepEndBy` comma)
      withoutparens = if nonparens
                      then func_arg expressionF `sepEndBy1` comma
                      else fail "Not an argument list allowed with function without parentheses"
  args  <- withparens <|> withoutparens
  pure (fname, V.fromList args)

literalValue :: Parser UnresolvedValue
literalValue = lexeme (fmap UString stringLiteral' <|> fmap UString bareword <|> fmap UNumber numericalvalue <?> "Literal Value")
  where
    signed :: Num n => Parser (n -> n)
    signed = (negate <$ char '-') <|> pure (\x -> x)
    numericalvalue = ((,) <$> signed <*> integerOrDouble) >>= \case
      (s, Left x)  -> pure (s (fromIntegral x))
      (s, Right y) -> pure (s (Scientific.fromFloatDigits y))

data TerminalMode
  = FunctionWithoutParens
  | StandardMode

-- this is a hack for functions :(
terminalG :: TerminalMode -> Parser Expression
terminalG mode =
      parens expression
  <|> fmap (Terminal . UInterpolable) interpolableString
  <|> (Terminal UUndef <$ reserved "undef")
  <|> fmap (Terminal . URegexp) termRegexp
  <|> varExpression
  <|> fmap Terminal puppetArray
  <|> fmap Terminal puppetHash
  <|> fmap (Terminal . UBoolean) puppetBool
  <|> case mode of
        FunctionWithoutParens -> remaining
        StandardMode          -> lambda <|> remaining
 where
   lambda = fmap Terminal (fmap UHOLambdaCall (try lambdaCall) <|> try funcCall)
   remaining = fmap (Terminal . UDataType) datatype
           <|> fmap Terminal resourceReference
           <|> fmap Terminal literalValue
   funcCall :: Parser UnresolvedValue
   funcCall = uncurry UFunctionCall <$> genFunctionCall False

regexp :: Parser Text
regexp = do
  void (single '/')
  Text.pack . concat <$> many ( do { void (char '\\') ; x <- anySingle; return ['\\', x] } <|> some (noneOf [ '/', '\\' ]) )
      <* symbolic '/'

compileRegexp :: Text -> Parser CompRegex
compileRegexp p = case Regex.compile' Regex.compBlank Regex.execBlank (encodeUtf8 p) of
  Right r -> pure $ CompRegex p r
  Left ms -> fail ("Can't parse regexp /" <> Text.unpack p <> "/ : " ++ show ms)

termRegexp :: Parser CompRegex
termRegexp = regexp >>= compileRegexp

terminal :: Parser Expression
terminal = terminalG StandardMode

expressionTable :: [[Operator Parser Expression]]
expressionTable = [ [ Postfix indexLookupChain ] -- http://stackoverflow.com/questions/10475337/parsec-expr-repeated-prefix-postfix-operator-not-supported
                  , [ Prefix ( symbolic '-'   *> pure Negate           ) ]
                  , [ Prefix ( symbolic '!'   *> pure Not              ) ]
                  , [ InfixL ( symbolic '.'   *> pure FunctionApplication ) ]
                  , [ InfixL ( reserved "in"  *> pure Contains         ) ]
                  , [ InfixL ( symbolic '/'   *> pure Division         )
                    , InfixL ( symbolic '*'   *> pure Multiplication   )
                    ]
                  , [ InfixL ( symbolic '+'   *> pure Addition     )
                    , InfixL ( symbolic '-'   *> pure Substraction )
                    ]
                  , [ InfixL ( symbol "<<"    *> pure LeftShift  )
                    , InfixL ( symbol ">>"    *> pure RightShift )
                    ]
                  , [ InfixL ( symbol "=="    *> pure Equal     )
                    , InfixL ( symbol "!="    *> pure Different )
                    ]
                  , [ InfixL ( symbol "=~"    *> pure RegexMatch    )
                    , InfixL ( symbol "!~"    *> pure NotRegexMatch )
                    ]
                  , [ InfixL ( symbol ">="    *> pure MoreEqualThan )
                    , InfixL ( symbol "<="    *> pure LessEqualThan )
                    , InfixL ( symbol ">"     *> pure MoreThan      )
                    , InfixL ( symbol "<"     *> pure LessThan      )
                    ]
                  , [ InfixL ( reserved "and" *> pure And )
                    , InfixL ( reserved "or"  *> pure Or  )
                    ]
                  ]

-- | Postfix of a chain of lookup indexes such as "['release']['major']"
indexLookupChain :: Parser (Expression -> Expression)
indexLookupChain = List.foldr1 (flip (.)) <$> some checkLookup
  where
    checkLookup = flip Lookup <$> brackets expression

stringExpression :: Parser Expression
stringExpression =
      (Terminal . UInterpolable) <$> interpolableString
  <|> (reserved "undef" $> Terminal UUndef)
  <|> (Terminal . UBoolean) <$> puppetBool
  <|> varExpression
  <|> Terminal <$> literalValue

-- | a = b = 0
chainedVariableReferences :: Parser [Text]
chainedVariableReferences = do
  h <- variableReference
  t <- many (try next)
  pure (h:t)
  where
    next = symbolic '=' *> variableReference <* lookAhead (single '=' *> space1)

varAssign :: Parser VarAssignDecl
varAssign = do
  p <- getSourcePos
  mt <- optional datatype
  vs <- chainedVariableReferences
  void $ symbolic '='
  expr <- expression
  pe <- getSourcePos
  pure (VarAssignDecl mt vs expr (p :!: pe))

nodeDecl :: Parser [NodeDecl]
nodeDecl = do
  p <- getSourcePos
  reserved "node"
  let toString (UString s) = s
      toString (UNumber n) = scientific2text n
      toString _           = panic "Can't happen at nodeDecl"
      nodename = (reserved "default" >> pure NodeDefault) <|> fmap (NodeName . toString) literalValue
  ns <- (fmap NodeMatch termRegexp <|> nodename) `sepBy1` comma
  inheritance <- option S.Nothing (fmap S.Just (reserved "inherits" *> nodename))
  st <- braces statementList
  pe <- getSourcePos
  pure [NodeDecl n st inheritance (p :!: pe) | n <- ns]

defineDecl :: Parser DefineDecl
defineDecl = do
  p <- getSourcePos
  reserved "define"
  name <- typeName
  -- TODO check native type
  params <- option V.empty puppetClassParameters
  st <- braces statementList
  pe <- getSourcePos
  pure (DefineDecl name params st (p :!: pe))

puppetClassParameters :: Parser Parameters
puppetClassParameters = V.fromList <$> parens (sepComma var)
  where
    toStrictMaybe (Just x) = S.Just x
    toStrictMaybe Nothing  = S.Nothing
    var :: Parser (Pair (Pair Text (S.Maybe UDataType)) (S.Maybe Expression))
    var = do
      tp <- toStrictMaybe <$> optional datatype
      n  <- variableReference
      df <- toStrictMaybe <$> optional (symbolic '=' *> expression)
      pure ((n :!: tp) :!: df)

puppetIfStyleCondition :: Parser (Pair Expression (Vector Statement))
puppetIfStyleCondition = (:!:) <$> expression <*> braces statementList

unlessCondition :: Parser ConditionalDecl
unlessCondition = do
  p <- getSourcePos
  reserved "unless"
  (cond :!: stmts) <- puppetIfStyleCondition
  elsecond <- option V.empty (reserved "else" *> braces statementList)
  let ec = if V.null elsecond
               then []
               else [Terminal (UBoolean True) :!: elsecond]
  pe <- getSourcePos
  pure (ConditionalDecl (V.fromList ((Not cond :!: stmts) : ec )) (p :!: pe))

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
  pure (ConditionalDecl (V.fromList (maincond : others ++ ec)) (p :!: pe))

caseCondition :: Parser ConditionalDecl
caseCondition = do
  let puppetRegexpCase = Terminal . URegexp <$> termRegexp
      defaultCase = Terminal (UBoolean True) <$ reserved "default"
      matchesToExpression e (x, stmts) = f x :!: stmts
        where f = case x of
                    (Terminal (UBoolean _)) -> identity
                    (Terminal (URegexp _))  -> RegexMatch e
                    _                       -> Equal e
      cases = do
        matches <- (puppetRegexpCase <|> defaultCase <|> expression) `sepBy1` comma
        void $ symbolic ':'
        stmts <- braces statementList
        pure $ map (,stmts) matches
  p <- getSourcePos
  reserved "case"
  expr1 <- expression
  condlist <- concat <$> braces (some cases)
  pe <- getSourcePos
  pure (ConditionalDecl (V.fromList (map (matchesToExpression expr1) condlist)) (p :!: pe) )

data OperatorChain a
  = OperatorChain a LinkType (OperatorChain a)
  | EndOfChain a

instance Foldable OperatorChain where
  foldMap f (EndOfChain x)         = f x
  foldMap f (OperatorChain a _ nx) = f a <> foldMap f nx

operatorChainStatement :: OperatorChain a -> a
operatorChainStatement (OperatorChain a _ _) = a
operatorChainStatement (EndOfChain x)        = x

zipChain :: OperatorChain a -> [ ( a, a, LinkType ) ]
zipChain (OperatorChain a d nx) = (a, operatorChainStatement nx, d) : zipChain nx
zipChain (EndOfChain _)         = []

depOperator :: Parser LinkType
depOperator =
      (RBefore <$ symbol "->")
  <|> (RNotify <$ symbol "~>")

assignment :: Parser AttributeDecl
assignment =
      (AttributeDecl <$> lexeme key <*> arrowOp  <*> expression)
  <|> (AttributeWildcard <$> (symbolic '*' *> symbol "=>" *> expression))
  where
    key = bareword <?> "Assignment key"
    arrowOp =
          (AssignArrow <$ symbol "=>")
      <|> (AppendArrow <$ symbol "+>")

-- | Resource Collector
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
  pure (ResCollDecl collectortype restype e (V.fromList overrides) (p :!: pe) )
  where
    searchExpression :: Parser SearchExpression
    searchExpression =
      let searchTable :: [[Operator Parser SearchExpression]]
          searchTable = [ [ InfixL ( reserved "and" *> pure AndSearch )
                          , InfixL ( reserved "or"  *> pure OrSearch  )
                          ] ]
          searchterm = parens searchExpression <|> check
          check = do
            attrib <- parameterName
            opr    <- (EqualitySearch <$ symbol "==")
                  <|> (NonEqualitySearch <$ symbol "!=")
            term   <- stringExpression
            pure (opr attrib term)
      in makeExprParser (lexeme searchterm) searchTable

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
  pure (MainFuncDecl fname args (p :!: pe))

hoLambdaDecl :: Parser HigherOrderLambdaDecl
hoLambdaDecl = do
  p <- getSourcePos
  fc <- lambdaCall
  pe <- getSourcePos
  pure (HigherOrderLambdaDecl fc (p :!: pe))

dotLambdaDecl :: Parser HigherOrderLambdaDecl
dotLambdaDecl = do
  p <- getSourcePos
  ex <- expression
  pe <- getSourcePos
  hf <- case ex of
    FunctionApplication e (Terminal (UHOLambdaCall hf)) -> do
      unless (null (hf ^. hoLambdaExpr)) (fail "Can't call a function with . and ()")
      pure (hf & hoLambdaExpr .~ V.singleton e)
    Terminal (UHOLambdaCall hf) -> do
      when (null (hf ^. hoLambdaExpr)) (fail "This function needs data to operate on")
      pure hf
    _ -> fail "A method chained by dots."
  pure (HigherOrderLambdaDecl hf (p :!: pe))


resDefaultDecl :: Parser ResDefaultDecl
resDefaultDecl = do
  p <- getSourcePos
  rnd  <- resourceNameRef
  let assignmentList = V.fromList <$> sepComma1 assignment
  asl <- braces assignmentList
  pe <- getSourcePos
  pure (ResDefaultDecl rnd asl (p :!: pe))

resOverrideDecl :: Parser [ResOverrideDecl]
resOverrideDecl = do
  p <- getSourcePos
  restype  <- resourceNameRef
  names <- brackets (expression `sepBy1` comma) <?> "Resource reference values"
  assignments <- V.fromList <$> braces (sepComma assignment)
  pe <- getSourcePos
  pure [ ResOverrideDecl restype n assignments (p :!: pe) | n <- names ]

arrayof :: Parser p -> Parser [p]
arrayof p = symbolic '[' *> sepBy p comma <* symbolic ']'

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
        lookAhead anySingle >>= \case
          '[' -> do
              resnames <- brackets (expression `sepBy1` comma)
              pe <- getSourcePos
              pure (ChainResRefr restype resnames (p :!: pe))
          _ -> ChainResColl <$> resCollDecl p restype
  let oneresource = pure <$> try withresname <|> map ChainResDecl <$> resDeclGroup
  chain <- parseRelationships (oneresource <|> concat <$> arrayof oneresource)
  let relations = do
        (g1, g2, lt) <- zipChain chain
        (rt1, rn1, _   :!: pe1) <- concatMap extractResRef g1
        (rt2, rn2, ps2 :!: _  ) <- concatMap extractResRef g2
        pure (DepDecl (rt1 :!: rn1) (rt2 :!: rn2) lt (pe1 :!: ps2))
  pure $ map DependencyDeclaration relations <> (chain ^.. folded . folded . to extractChainStatement . folded)
  where
    extractResRef :: ChainableRes -> [(Text, Expression, PPosition)]
    extractResRef (ChainResColl _)                      = []
    extractResRef (ChainResDecl (ResDecl rt rn _ _ pp)) = [(rt,rn,pp)]
    extractResRef (ChainResRefr rt rns pp)              = [(rt,rn,pp) | rn <- rns]

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
            names <- brackets (sepComma1 resourceName) <|> fmap pure resourceName
            void $ symbolic ':'
            vals  <- fmap V.fromList (sepComma assignment)
            pe <- getSourcePos
            pure [(n, vals, p :!: pe) | n <- names ]
          groupDeclaration = (,) <$> many (char '@') <*> typeName <* symbolic '{'
      (virts, rtype) <- try groupDeclaration -- for matching reasons, this gets a try until the opening brace
      let sep = symbolic ';' <|> comma
      x <- resourceDeclaration `sepEndBy1` sep
      void $ symbolic '}'
      virtuality <- case virts of
        ""   -> pure Normal
        "@"  -> pure Virtual
        "@@" -> pure Exported
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
  <|> (pure . HigherOrderLambdaDeclaration <$> try hoLambdaDecl)
  <|> (pure . MainFunctionDeclaration <$> mainFuncDecl)
  <?> "Statement"

datatype :: Parser UDataType
datatype =
      dtString
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
  <|> (UDTDeferred <$ reserved "Deferred")
  <|> (reserved "Sensitive" *> (UDTSensitive <$> brackets datatype))
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
        [minlen]        -> return $ constructor (Just minlen) Nothing
        [minlen,maxlen] -> return $ constructor (Just minlen) (Just maxlen)
        _               -> fail ("Too many arguments to datatype " ++ Text.unpack s)
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
        Nothing                 -> return (UDTArray UDTData 0 Nothing)
        Just (t, Nothing)       -> return (UDTArray t 0 Nothing)
        Just (t, Just [mi])     -> return (UDTArray t mi Nothing)
        Just (t, Just [mi, mx]) -> return (UDTArray t mi (Just mx))
        Just (_, Just _)        -> fail "Too many arguments to datatype Array"
    dtHash = do
      reserved "Hash"
      ml <- optional $ brackets $ do
        tk <- datatype
        symbolic ','
        tv <- datatype
        rst <- optional (symbolic ',' *> integer `sepBy1` symbolic ',')
        return (tk, tv, rst)
      case ml of
        Nothing                      -> return (UDTHash UDTScalar UDTData 0 Nothing)
        Just (tk, tv, Nothing)       -> return (UDTHash tk tv 0 Nothing)
        Just (tk, tv, Just [mi])     -> return (UDTHash tk tv mi Nothing)
        Just (tk, tv, Just [mi, mx]) -> return (UDTHash tk tv mi (Just mx))
        Just (_, _, Just _)          -> fail "Too many arguments to datatype Hash"
    dtExternal =
      choice [ reserved "Stdlib::Absolutepath" $> UDTData
             , reserved "Stdlib::Base32" $> UDTData
             , reserved "Stdlib::Base64" $> UDTData
             , reserved "Stdlib::Compat::Absolute_path" $> UDTData
             , reserved "Stdlib::Compat::Array" $> UDTData
             , reserved "Stdlib::Compat::Bool" $> UDTData
             , reserved "Stdlib::Compat::Float" $> UDTData
             , reserved "Stdlib::Compat::Hash" $> UDTData
             , reserved "Stdlib::Compat::Integer" $> UDTData
             , reserved "Stdlib::Compat::Ip_address" $> UDTData
             , reserved "Stdlib::Compat::Ipv4" $> UDTData
             , reserved "Stdlib::Compat::Ipv6" $> UDTData
             , reserved "Stdlib::Compat::Numeric" $> UDTData
             , reserved "Stdlib::Compat::String" $> UDTData
             , reserved "Stdlib::Ensure::Service" $> UDTData
             , reserved "Stdlib::Filemode" $> UDTData
             , reserved "Stdlib::Filesource" $> UDTData
             , reserved "Stdlib::Fqdn" $> UDTData
             , reserved "Stdlib::Host" $> UDTData
             , reserved "Stdlib::HTTPSUrl" $> UDTData
             , reserved "Stdlib::HTTPUrl" $> UDTData
             , reserved "Stdlib::IP::Address::Nosubnet" $> UDTData
             , reserved "Stdlib::Ip_address" $> UDTData
             , reserved "Stdlib::IP::Address" $> UDTData
             , reserved "Stdlib::IP::Address::V4::CIDR" $> UDTData
             , reserved "Stdlib::IP::Address::V4::Nosubnet" $> UDTData
             , reserved "Stdlib::IP::Address::V4" $> UDTData
             , reserved "Stdlib::IP::Address::V6::Alternative" $> UDTData
             , reserved "Stdlib::IP::Address::V6::Compressed" $> UDTData
             , reserved "Stdlib::IP::Address::V6::Full" $> UDTData
             , reserved "Stdlib::IP::Address::V6::Nosubnet::Alternative" $> UDTData
             , reserved "Stdlib::IP::Address::V6::Nosubnet::Compressed" $> UDTData
             , reserved "Stdlib::IP::Address::V6::Nosubnet::Full" $> UDTData
             , reserved "Stdlib::IP::Address::V6::Nosubnet" $> UDTData
             , reserved "Stdlib::IP::Address::V6" $> UDTData
             , reserved "Stdlib::Ipv4" $> UDTData
             , reserved "Stdlib::Ipv6" $> UDTData
             , reserved "Stdlib::MAC" $> UDTData
             , reserved "Stdlib::Port::Privileged" $> UDTData
             , reserved "Stdlib::Port" $> UDTData
             , reserved "Stdlib::Port::Unprivileged" $> UDTData
             , reserved "Stdlib::Unixpath" $> UDTData
             , reserved "Stdlib::Windowspath" $> UDTData
             , reserved "Nginx::ErrorLogSeverity" $> UDTData
             , reserved "Jenkins::Tunnel" $> UDTData
             , reserved "Systemd::Unit" $> UDTData
             , reserved "Systemd::ServiceLimits" $> UDTData
             , reserved "Systemd::Dropin" $> UDTData
             , reserved "Systemd::JournaldSettings" $> UDTData
             , reserved "Systemd::JournaldSettings::Ensure" $> UDTData
             , reserved "Systemd::LogindSettings" $> UDTData
             , reserved "Systemd::LogindSettings::Ensure" $> UDTData
             , reserved "Systemd::CoredumpSettings" $> UDTData
             ]

statementList :: Parser (Vector Statement)
statementList = V.fromList . concat <$> many statement

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
        lambdaParameter = LambdaParam <$> optional datatype <*> lexeme (char '$' *> identifier)
        hp = V.fromList <$> lambdaParameter `sepBy1` comma
