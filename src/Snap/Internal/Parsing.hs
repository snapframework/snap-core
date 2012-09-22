{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Internal.Parsing where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Applicative
import           Control.Arrow (first, second)
import           Control.Monad
import           Data.Attoparsec.Char8
import           Data.Bits
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.CaseInsensitive as CI
import           Data.CaseInsensitive (CI)
import           Data.Char hiding (isDigit, isSpace)
import           Data.Int
import           Data.List (intersperse)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           GHC.Exts
import           GHC.Word (Word8(..))
import           Prelude hiding (head, take, takeWhile)
------------------------------------------------------------------------------
import           Snap.Internal.Http.Types
import           Snap.Internal.Parsing.FastSet (FastSet)
import qualified Snap.Internal.Parsing.FastSet as FS


------------------------------------------------------------------------------
{-# INLINE fullyParse #-}
fullyParse :: ByteString -> Parser a -> Either String a
fullyParse s p =
    case r' of
      (Fail _ _ e) -> Left e
      (Partial _)  -> Left "parse failed"
      (Done _ x)   -> Right x
  where
    r  = parse p s
    r' = feed r ""


------------------------------------------------------------------------------
parseNum :: Parser Int64
parseNum = decimal


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
generateFS :: (Word8 -> Bool) -> FastSet
generateFS f = FS.fromList $ filter f [0..255]


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
    isFieldChar = flip FS.memberChar fieldCharSet


------------------------------------------------------------------------------
fieldCharSet :: FastSet
fieldCharSet = generateFS f
  where
    f d = let c = (toEnum $ fromEnum d)
          in (isDigit c) || (isAlpha c) || c == '-' || c == '_'


------------------------------------------------------------------------------
-- | Parser for request headers.
pHeaders :: Parser [(ByteString, ByteString)]
pHeaders = many header
  where
    --------------------------------------------------------------------------
    header            = {-# SCC "pHeaders/header" #-}
                        liftA2 (,)
                            fieldName
                            (char ':' *> spaces *> contents)

    --------------------------------------------------------------------------
    fieldName         = {-# SCC "pHeaders/fieldName" #-}
                        liftA2 S.cons letter fieldChars

    --------------------------------------------------------------------------
    contents          = {-# SCC "pHeaders/contents" #-}
                        liftA2 S.append
                            (untilEOL <* crlf)
                            (continuation <|> pure S.empty)

    --------------------------------------------------------------------------
    isLeadingWS w     = {-# SCC "pHeaders/isLeadingWS" #-}
                        w == ' ' || w == '\t'

    --------------------------------------------------------------------------
    leadingWhiteSpace = {-# SCC "pHeaders/leadingWhiteSpace" #-}
                        takeWhile1 isLeadingWS

    --------------------------------------------------------------------------
    continuation      = {-# SCC "pHeaders/continuation" #-}
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

    q      = char '\"'
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
    return $! a:b


------------------------------------------------------------------------------
{-# INLINE pAvPair #-}
pAvPair :: Parser (ByteString, ByteString)
pAvPair = do
    key <- pToken <* pSpaces
    val <- liftM trim (option "" $ char '=' *> pSpaces *> pWord)
    return $! (key, val)


------------------------------------------------------------------------------
pParameter :: Parser (ByteString, ByteString)
pParameter = do
    key <- pToken <* pSpaces
    val <- liftM trim (char '=' *> pSpaces *> pWord)
    return $! (trim key, val)


------------------------------------------------------------------------------
{-# INLINE trim #-}
trim :: ByteString -> ByteString
trim = snd . S.span isSpace . fst . S.spanEnd isSpace


------------------------------------------------------------------------------
pValueWithParameters :: Parser (ByteString, [(CI ByteString, ByteString)])
pValueWithParameters = do
    value  <- liftM trim (pSpaces *> takeWhile (/= ';'))
    params <- many pParam
    return (value, map (first CI.mk) params)

  where
    pParam = pSpaces *> char ';' *> pSpaces *> pParameter


------------------------------------------------------------------------------
pContentTypeWithParameters :: Parser ( ByteString
                                     , [(CI ByteString, ByteString)] )
pContentTypeWithParameters = do
    value  <- liftM trim (pSpaces *> takeWhile (not . isSep))
    params <- many (pSpaces *> satisfy isSep *> pSpaces *> pParameter)
    return $! (value, map (first CI.mk) params)

  where
    isSep c = c == ';' || c == ','


------------------------------------------------------------------------------
{-# INLINE pToken #-}
pToken :: Parser ByteString
pToken = takeWhile isToken


------------------------------------------------------------------------------
{-# INLINE isToken #-}
isToken :: Char -> Bool
isToken c = FS.memberChar c tokenTable


------------------------------------------------------------------------------
tokenTable :: FastSet
tokenTable = generateFS (f . toEnum . fromEnum)
  where
    f = matchAll [ isAscii
                 , not . isControl
                 , not . isSpace
                 , not . flip elem [ '(', ')', '<', '>', '@', ',', ';'
                                   , ':', '\\', '\"', '/', '[', ']'
                                   , '?', '=', '{', '}' ]
                 ]


                              ------------------
                              -- Url encoding --
                              ------------------

------------------------------------------------------------------------------
{-# INLINE parseToCompletion #-}
parseToCompletion :: Parser a -> ByteString -> Maybe a
parseToCompletion p s = toResult $ finish r
  where
    r = parse p s

    toResult (Done _ c) = Just c
    toResult _          = Nothing


------------------------------------------------------------------------------
type DList a = [a] -> [a]

pUrlEscaped :: Parser ByteString
pUrlEscaped = do
    sq <- nextChunk id
    return $! S.concat $ sq []

  where
    --------------------------------------------------------------------------
    nextChunk :: DList ByteString -> Parser (DList ByteString)
    nextChunk !s = (endOfInput *> pure s) <|> do
        c <- anyChar
        case c of
          '+' -> plusSpace s
          '%' -> percentEncoded s
          _   -> unEncoded c s

    --------------------------------------------------------------------------
    percentEncoded :: DList ByteString -> Parser (DList ByteString)
    percentEncoded !l = do
        hx <- take 2
        when (S.length hx /= 2 || (not $ S.all isHexDigit hx)) $
             fail "bad hex in url"

        let code = w2c ((unsafeFromHex hx) :: Word8)
        nextChunk $ l . ((S.singleton code) :)

    --------------------------------------------------------------------------
    unEncoded :: Char -> DList ByteString -> Parser (DList ByteString)
    unEncoded !c !l' = do
        let l = l' . ((S.singleton c) :)
        bs   <- takeTill (flip elem "%+")
        if S.null bs
          then nextChunk l
          else nextChunk $ l . (bs :)

    --------------------------------------------------------------------------
    plusSpace :: DList ByteString -> Parser (DList ByteString)
    plusSpace l = nextChunk (l . ((S.singleton ' ') :))


------------------------------------------------------------------------------
-- "...Only alphanumerics [0-9a-zA-Z], the special characters "$-_.+!*'(),"
-- [not including the quotes - ed], and reserved characters used for their
-- reserved purposes may be used unencoded within a URL."


------------------------------------------------------------------------------
-- | Decodes an URL-escaped string (see
-- <http://tools.ietf.org/html/rfc2396.html#section-2.4>)
urlDecode :: ByteString -> Maybe ByteString
urlDecode = parseToCompletion pUrlEscaped
{-# INLINE urlDecode #-}


------------------------------------------------------------------------------
-- | URL-escapes a string (see
-- <http://tools.ietf.org/html/rfc2396.html#section-2.4>)
urlEncode :: ByteString -> ByteString
urlEncode = toByteString . urlEncodeBuilder
{-# INLINE urlEncode #-}


------------------------------------------------------------------------------
-- | URL-escapes a string (see
-- <http://tools.ietf.org/html/rfc2396.html#section-2.4>) into a 'Builder'.
urlEncodeBuilder :: ByteString -> Builder
urlEncodeBuilder = go mempty
  where
    go !b !s = maybe b' esc (S.uncons y)
      where
        (x,y)     = S.span (flip FS.memberChar urlEncodeTable) s
        b'        = b `mappend` fromByteString x
        esc (c,r) = let b'' = if c == ' '
                                then b' `mappend` fromWord8 (c2w '+')
                                else b' `mappend` hexd c
                    in go b'' r


------------------------------------------------------------------------------
urlEncodeTable :: FastSet
urlEncodeTable = generateFS f
  where
    f c = any ($ (w2c c)) [isAlphaNum, flip elem "$-.!*'(),"]


------------------------------------------------------------------------------
hexd :: Char -> Builder
hexd c0 = fromWord8 (c2w '%') `mappend` fromWord8 hi `mappend` fromWord8 low
  where
    !c        = c2w c0
    toDigit   = c2w . intToDigit
    !low      = toDigit $ fromEnum $ c .&. 0xf
    !hi       = toDigit $ (c .&. 0xf0) `shiftr` 4

    shiftr (W8# a#) (I# b#) = I# (word2Int# (uncheckedShiftRL# a# b#))


------------------------------------------------------------------------------
finish :: Result a -> Result a
finish (Partial f) = flip feed "" $ f ""
finish x           = x


                    ---------------------------------------
                    -- application/x-www-form-urlencoded --
                    ---------------------------------------

------------------------------------------------------------------------------
-- | Parses a string encoded in @application/x-www-form-urlencoded@ format.
parseUrlEncoded :: ByteString -> Map ByteString [ByteString]
parseUrlEncoded s = foldr ins Map.empty decoded

  where
    --------------------------------------------------------------------------
    ins (!k,v) !m = Map.insertWith' (++) k [v] m

    --------------------------------------------------------------------------
    parts :: [(ByteString,ByteString)]
    parts = map breakApart $
            S.splitWith (\c -> c == '&' || c == ';') s

    --------------------------------------------------------------------------
    breakApart = (second (S.drop 1)) . S.break (== '=')

    --------------------------------------------------------------------------
    urldecode = parseToCompletion pUrlEscaped

    --------------------------------------------------------------------------
    decodeOne (a,b) = do
        !a' <- urldecode a
        !b' <- urldecode b
        return $! (a',b')

    --------------------------------------------------------------------------
    decoded = go id parts
      where
        go !dl []     = dl []
        go !dl (x:xs) = maybe (go dl xs)
                              (\p -> go (dl . (p:)) xs)
                              (decodeOne x)


------------------------------------------------------------------------------
buildUrlEncoded :: Map ByteString [ByteString] -> Builder
buildUrlEncoded m = mconcat builders
  where
    builders        = intersperse (fromWord8 $ c2w '&') $
                      concatMap encodeVS $ Map.toList m

    encodeVS (k,vs) = map (encodeOne k) vs

    encodeOne k v   = mconcat [ urlEncodeBuilder k
                              , fromWord8 $ c2w '='
                              , urlEncodeBuilder v ]


------------------------------------------------------------------------------
printUrlEncoded :: Map ByteString [ByteString] -> ByteString
printUrlEncoded = toByteString . buildUrlEncoded


                             --------------------
                             -- Cookie parsing --
                             --------------------

------------------------------------------------------------------------------
-- these definitions try to mirror RFC-2068 (the HTTP/1.1 spec) and RFC-2109
-- (cookie spec): please point out any errors!
------------------------------------------------------------------------------
pCookies :: Parser [Cookie]
pCookies = do
    -- grab kvps and turn to strict bytestrings
    kvps <- pAvPairs
    return $! map toCookie $ filter (not . S.isPrefixOf "$" . fst) kvps

  where
    toCookie (nm,val) = Cookie nm val Nothing Nothing Nothing False False


------------------------------------------------------------------------------
parseCookie :: ByteString -> Maybe [Cookie]
parseCookie = parseToCompletion pCookies


                            -----------------------
                            -- utility functions --
                            -----------------------

------------------------------------------------------------------------------
strictize :: L.ByteString -> ByteString
strictize = S.concat . L.toChunks

------------------------------------------------------------------------------
unsafeFromHex :: (Enum a, Num a, Bits a) => ByteString -> a
unsafeFromHex = S.foldl' f 0
  where
#if MIN_VERSION_base(4,5,0)
    sl = unsafeShiftL
#else
    sl = shiftL
#endif

    f !cnt !i = sl cnt 4 .|. nybble i

    nybble c | c >= '0' && c <= '9' = toEnum $! fromEnum c - fromEnum '0'
             | c >= 'a' && c <= 'f' = toEnum $! 10 + fromEnum c - fromEnum 'a'
             | c >= 'A' && c <= 'F' = toEnum $! 10 + fromEnum c - fromEnum 'A'
             | otherwise            = error $ "bad hex digit: " ++ show c
{-# INLINE unsafeFromHex #-}


------------------------------------------------------------------------------
unsafeFromInt :: (Enum a, Num a, Bits a) => ByteString -> a
unsafeFromInt = S.foldl' f 0
  where
    f !cnt !i = cnt * 10 + toEnum (digitToInt i)
{-# INLINE unsafeFromInt #-}
