{-# LANGUAGE OverloadedStrings #-}

module Snap.Internal.Parsing where

import           Control.Arrow (first)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.CIByteString
import           Data.Char (isAlpha, isAscii, isControl)
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Char8 hiding (Done, many)
import qualified Data.Attoparsec.Char8 as Atto
import           Data.ByteString.Nums.Careless.Int (int)
import           Data.Int
import qualified Data.Vector.Unboxed as Vec
import           Data.Vector.Unboxed (Vector)
import           Prelude hiding (head, take, takeWhile)


------------------------------------------------------------------------------
fullyParse :: ByteString -> Parser a -> Either String a
fullyParse s p =
    case r' of
      (Fail _ _ e)    -> Left e
      (Partial _)     -> Left "parse failed"
      (Atto.Done _ x) -> Right x
  where
    r  = parse p s
    r' = feed r ""


------------------------------------------------------------------------------
parseNum :: Parser Int64
parseNum = liftM int $ Atto.takeWhile1 Atto.isDigit


------------------------------------------------------------------------------
-- | Parsers for different tokens in an HTTP request.
sp, digit, letter :: Parser Char
sp       = char ' '
digit    = satisfy isDigit
letter   = satisfy isAlpha


------------------------------------------------------------------------------
untilEOL :: Parser ByteString
untilEOL = takeWhile notend
  where
    notend c = not $ c == '\r' || c == '\n'


------------------------------------------------------------------------------
crlf :: Parser ByteString
crlf = string "\r\n"


------------------------------------------------------------------------------
-- | Parser for zero or more spaces.
spaces :: Parser [Char]
spaces = many sp


------------------------------------------------------------------------------
pSpaces :: Parser ByteString
pSpaces = takeWhile isSpace


------------------------------------------------------------------------------
fieldChars :: Parser ByteString
fieldChars = takeWhile isFieldChar
  where
    isFieldChar c = (Vec.!) fieldCharTable (fromEnum c)


------------------------------------------------------------------------------
fieldCharTable :: Vector Bool
fieldCharTable = Vec.generate 256 f
  where
    f d = let c=toEnum d in (isDigit c) || (isAlpha c) || c == '-' || c == '_'


------------------------------------------------------------------------------
-- | Parser for request headers.
pHeaders :: Parser [(ByteString, ByteString)]
pHeaders = many header
  where
    header = {-# SCC "pHeaders/header" #-}
             liftA2 (,)
                 fieldName
                 (char ':' *> spaces *> contents)

    fieldName = {-# SCC "pHeaders/fieldName" #-}
                liftA2 S.cons letter fieldChars

    contents = {-# SCC "pHeaders/contents" #-}
               liftA2 S.append
                   (untilEOL <* crlf)
                   (continuation <|> pure S.empty)

    isLeadingWS w = {-# SCC "pHeaders/isLeadingWS" #-}
                    elem w wstab

    wstab = " \t"

    leadingWhiteSpace = {-# SCC "pHeaders/leadingWhiteSpace" #-}
                        takeWhile1 isLeadingWS

    continuation = {-# SCC "pHeaders/continuation" #-}
                   liftA2 S.cons
                          (leadingWhiteSpace *> pure ' ')
                          contents


------------------------------------------------------------------------------
-- unhelpfully, the spec mentions "old-style" cookies that don't have quotes
-- around the value. wonderful.
pWord :: Parser ByteString
pWord = pQuotedString <|> (takeWhile (/= ';'))


------------------------------------------------------------------------------
pQuotedString :: Parser ByteString
pQuotedString = q *> quotedText <* q
  where
    quotedText = (S.concat . reverse) <$> f []

    f soFar = do
        t <- takeWhile qdtext

        let soFar' = t:soFar

        -- RFC says that backslash only escapes for <">
        choice [ string "\\\"" *> f ("\"" : soFar')
               , pure soFar' ]


    q = char '\"'

    qdtext = matchAll [ isRFCText, (/= '\"'), (/= '\\') ]


------------------------------------------------------------------------------
{-# INLINE isRFCText #-}
isRFCText :: Char -> Bool
isRFCText = not . isControl


------------------------------------------------------------------------------
{-# INLINE matchAll #-}
matchAll :: [ Char -> Bool ] -> Char -> Bool
matchAll x c = and $ map ($ c) x


------------------------------------------------------------------------------
pAvPairs :: Parser [(ByteString, ByteString)]
pAvPairs = do
    a <- pAvPair
    b <- many (pSpaces *> char ';' *> pSpaces *> pAvPair)

    return $ a:b


------------------------------------------------------------------------------
pAvPair :: Parser (ByteString, ByteString)
pAvPair = do
    key <- pToken <* pSpaces
    val <- liftM trim (option "" $ char '=' *> pSpaces *> pWord)

    return (key, val)


------------------------------------------------------------------------------
pParameter :: Parser (ByteString, ByteString)
pParameter = do
    key <- pToken <* pSpaces
    val <- liftM trim (char '=' *> pSpaces *> pWord)
    return (trim key, val)


------------------------------------------------------------------------------
trim :: ByteString -> ByteString
trim = snd . S.span isSpace . fst . S.spanEnd isSpace


------------------------------------------------------------------------------
pValueWithParameters :: Parser (ByteString, [(CIByteString, ByteString)])
pValueWithParameters = do
    value  <- liftM trim (pSpaces *> takeWhile (/= ';'))
    params <- many pParam
    return (value, map (first toCI) params)
  where
    pParam = pSpaces *> char ';' *> pSpaces *> pParameter

------------------------------------------------------------------------------
pContentTypeWithParameters ::
    Parser (ByteString, [(CIByteString, ByteString)])
pContentTypeWithParameters = do
    value  <- liftM trim (pSpaces *> takeWhile (not . isSep))
    params <- many (pSpaces *> satisfy isSep *> pSpaces *> pParameter)
    return (value, map (first toCI) params)
  where
    isSep c = c == ';' || c == ','

------------------------------------------------------------------------------
pToken :: Parser ByteString
pToken = takeWhile isToken


------------------------------------------------------------------------------
{-# INLINE isToken #-}
isToken :: Char -> Bool
isToken c = (Vec.!) tokenTable (fromEnum c)
  where
    tokenTable :: Vector Bool
    tokenTable = Vec.generate 256 (f . toEnum)

    f = matchAll [ isAscii
                 , not . isControl
                 , not . isSpace
                 , not . flip elem [ '(', ')', '<', '>', '@', ',', ';'
                                   , ':', '\\', '\"', '/', '[', ']'
                                   , '?', '=', '{', '}' ]
                 ]


------------------------------------------------------------------------------
-- utility functions
------------------------------------------------------------------------------


------------------------------------------------------------------------------
strictize :: L.ByteString -> ByteString
strictize         = S.concat . L.toChunks
