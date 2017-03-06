{-# LANGUAGE TypeApplications #-}
module Puppet.Interpreter.Resolve.Sprintf where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Scientific (Scientific)
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.Builder            as TB
import qualified Data.Text.Lazy.Builder.Int        as TB
import qualified Data.Text.Lazy.Builder.Scientific as TB


import           Puppet.Interpreter.Types
import           Puppet.Interpreter.Utils
import           Puppet.Utils
import           Puppet.PP (pretty)
import           Puppet.Interpreter.PrettyPrinter()

data Flag = Minus | Plus | Space | Zero | Hash
          deriving (Show, Eq)

data FLen = Lhh | Lh | Ll | Lll | LL | Lz | Lj | Lt
           deriving (Show, Eq)

data FType = TPct | Td | Tu | Tf | TF | Te | TE | Tg | TG | Tx | TX | To | Ts | Tc | Tp | Ta | TA
           deriving (Show, Eq)

data PrintfFormat = PrintfFormat { _pfFlags :: [Flag]
                                 , _pfWidth :: Maybe Int
                                 , _pfPrec  :: Maybe Int
                                 , _pfLen   :: Maybe FLen
                                 , _pfType  :: FType
                                 } deriving (Show, Eq)

data FormatStringPart = Raw T.Text
                      | Format PrintfFormat
                      deriving (Show, Eq)

parseFormat :: T.Text -> [FormatStringPart]
parseFormat t | T.null t = []
              | T.null nxt = [Raw raw]
              | otherwise = Raw raw : rformat
  where
    (raw, nxt) = T.break (== '%') t
    tryNext = case parseFormat (T.tail nxt) of
                  (Raw nt : nxt') -> Raw (T.cons '%' nt) : nxt'
                  nxt' -> Raw (T.singleton '%') : nxt'
    rformat = case parse format nxt of
                  Fail _ _ _ -> tryNext
                  Partial _ -> tryNext
                  Done remaining f -> Format f : parseFormat remaining

flag :: Parser Flag
flag =   (Minus <$ char '-')
     <|> (Plus  <$ char '+')
     <|> (Space <$ char ' ')
     <|> (Zero  <$ char '0')
     <|> (Hash  <$ char '#')

lenModifier :: Parser FLen
lenModifier =   (Lhh <$ string "hh")
            <|> (Lh  <$ char 'h')
            <|> (Lll <$ string "ll")
            <|> (Ll  <$ char 'l')
            <|> (LL  <$ char 'L')
            <|> (Lz  <$ char 'z')
            <|> (Lj  <$ char 'j')
            <|> (Lt  <$ char 't')

ftype :: Parser FType
ftype =   (TPct <$ char '%')
      <|> (Td <$ char 'd')
      <|> (Td <$ char 'i')
      <|> (Tu <$ char 'u')
      <|> (Tf <$ char 'f')
      <|> (TF <$ char 'F')
      <|> (Te <$ char 'e')
      <|> (TE <$ char 'E')
      <|> (Tg <$ char 'g')
      <|> (TG <$ char 'G')
      <|> (Tx <$ char 'x')
      <|> (TX <$ char 'X')
      <|> (To <$ char 'o')
      <|> (Ts <$ char 's')
      <|> (Tc <$ char 'c')
      <|> (Ta <$ char 'a')
      <|> (Tp <$ char 'p')
      <|> (TA <$ char 'A')

format :: Parser PrintfFormat
format = do
    void $ char '%'
    flags <- many flag
    width <- optional decimal
    prec <- optional $ do
        void $ char '.'
        decimal
    len <- optional lenModifier
    ft <- ftype
    return (PrintfFormat flags width prec len ft)

sprintf :: T.Text -> [PValue] -> InterpreterMonad PValue
sprintf str oargs = PString . TL.toStrict . TB.toLazyText . mconcat <$> go (parseFormat str) oargs
  where
    go (Raw x : xs) args = (TB.fromText x :) <$> go xs args
    go (Format f : _) _ | Hash `elem` _pfFlags f = throwPosError "sprintf: the # modifier is not supported"
    go (Format f : xs) (arg : args) = do
        let numeric = case arg of
                          PNumber n -> pure n
                          PString s -> maybe (throwError "sprintf: Don't know how to convert this to a number") return (text2Scientific s)
                          _         -> throwError "sprintf: Don't know how to convert this to a number"
            flags = _pfFlags f
            sh mkBuilder n | has Minus            = TL.justifyLeft padlen ' ' (sprefix <> content)
                           | has Plus && has Zero = sprefix <> TL.justifyRight mpadlen '0' content
                           | has Plus             = TL.justifyRight padlen ' ' (sprefix <> content)
                           | has Zero             = TL.justifyRight padlen '0' content
                           | otherwise            = TL.justifyRight padlen ' ' content
                 where
                   (mpadlen, sprefix) | Plus  `elem` flags && n >= 0 = (padlen - 1, "+")
                                      | Space `elem` flags && n >= 0 = (padlen - 1, " ")
                                      | otherwise = (padlen, mempty)
                   padlen = maybe 0 fromIntegral (_pfWidth f)
                   has flg = flg `elem` flags
                   content = TB.toLazyText (mkBuilder n)
        baseString <- case _pfType f of
                          Td -> sh (TB.formatScientificBuilder TB.Fixed    (Just 0))    <$> numeric
                          Tf -> sh (TB.formatScientificBuilder TB.Fixed    (_pfPrec f)) <$> numeric
                          TF -> sh (TB.formatScientificBuilder TB.Fixed    (_pfPrec f)) <$> numeric
                          Tg -> sh (TB.formatScientificBuilder TB.Generic  (_pfPrec f)) <$> numeric
                          TG -> sh (TB.formatScientificBuilder TB.Generic  (_pfPrec f)) <$> numeric
                          Te -> sh (TB.formatScientificBuilder TB.Exponent (_pfPrec f)) <$> numeric
                          TE -> sh (TB.formatScientificBuilder TB.Exponent (_pfPrec f)) <$> numeric
                          Tx -> sh (TB.hexadecimal . truncate @Scientific @Integer)     <$> numeric
                          TX -> sh (TB.hexadecimal . truncate @Scientific @Integer)     <$> numeric
                          Ts -> return $ case arg of
                                             PString s -> TL.fromStrict s
                                             _ -> TL.pack (show (pretty arg))
                          _ -> throwPosError "sprintf: not yet supported"
        (TB.fromLazyText baseString :) <$> go xs args
    go [] [] = return []
    go _ [] = throwPosError "sprintf: not enough arguments"
    go [] _ = [] <$ let msg = "sprintf: too many arguments" in checkStrict msg msg

