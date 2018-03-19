{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Snap.Internal.Parsing where
------------------------------------------------------------------------------
import           Control.Applicative              (Alternative ((<|>)), Applicative (pure, (*>), (<*)), liftA2, (<$>))
import           Control.Arrow                    (first, second)
import           Control.Monad                    (Monad (return), MonadPlus (mzero), liftM, when)
import           Data.Attoparsec.ByteString.Char8 (IResult (Done, Fail, Partial), Parser, Result, anyChar, char, choice, decimal, endOfInput, feed, inClass, isDigit, isSpace, letter_ascii, many', match, option, parse, satisfy, skipSpace, skipWhile, string, take, takeTill, takeWhile, sepBy')
import qualified Data.Attoparsec.ByteString.Char8 as AP
import           Data.Bits                        (Bits (unsafeShiftL, (.&.), (.|.)))
import           Data.ByteString.Builder          (Builder, byteString, char8, toLazyByteString, word8)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as S
import           Data.ByteString.Internal         (c2w, w2c)
import qualified Data.ByteString.Lazy.Char8       as L
import           Data.CaseInsensitive             (CI)
import qualified Data.CaseInsensitive             as CI (mk)
import           Data.Char                        (Char, intToDigit, isAlpha, isAlphaNum, isAscii, isControl, isHexDigit, ord)
import           Data.Int                         (Int64)
import           Data.List                        (concat, intercalate, intersperse)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map (empty, insertWith, toList)
import           Data.Maybe                       (Maybe (..), maybe)
import           Data.Monoid                      (Monoid (mconcat, mempty), (<>))
import           Data.Word                        (Word8)
import           GHC.Exts                         (Int (I#), uncheckedShiftRL#, word2Int#)
import           GHC.Word                         (Word8 (..))
import           Prelude                          (Bool (..), Either (..), Enum (fromEnum, toEnum), Eq (..), Num (..), Ord (..), String, and, any, concatMap, elem, error, filter, flip, foldr, fst, id, map, not, otherwise, show, snd, ($), ($!), (&&), (++), (.), (||))
import           Snap.Internal.Http.Types         (Cookie (Cookie))
------------------------------------------------------------------------------


------------------------------------------------------------------------------
{-# INLINE fullyParse #-}
fullyParse :: ByteString -> Parser a -> Either String a
fullyParse = fullyParse' parse feed

{-# INLINE (<?>) #-}
(<?>) :: Parser a -> String -> Parser a
(<?>) a !b = (AP.<?>) a b
infix 0 <?>

------------------------------------------------------------------------------
{-# INLINE fullyParse' #-}
fullyParse' :: (Parser a -> ByteString -> Result a)
            -> (Result a -> ByteString -> Result a)
            -> ByteString
            -> Parser a
            -> Either String a
fullyParse' parseFunc feedFunc s p =
    case r' of
      (Fail _ context e) -> Left $ concat [ "Parsing "
                                          , intercalate "/" context
                                          , ": "
                                          , e
                                          , "."
                                          ]
      (Partial _)  -> Left "parse failed"  -- expected to be impossible
      (Done _ x)   -> Right x
  where
    r  = parseFunc p s
    r' = feedFunc r ""

------------------------------------------------------------------------------
-- Parsers for different tokens in an HTTP request.

------------------------------------------------------------------------------
parseNum :: Parser Int64
parseNum = decimal


------------------------------------------------------------------------------
untilEOL :: Parser ByteString
untilEOL = takeWhile notend <?> "untilEOL"
  where
    notend c = not $ c == '\r' || c == '\n'


------------------------------------------------------------------------------
crlf :: Parser ByteString
crlf = string "\r\n" <?> "crlf"


------------------------------------------------------------------------------
toTableList :: (Char -> Bool) -> [Char]
toTableList f = l
  where
    g c = c /= '-' && f c
    !l1 = filter g $ map w2c [0..255]
    !l0 = if f '-' then ['-'] else []
    !l  = l0 ++ l1
{-# INLINE toTableList #-}


------------------------------------------------------------------------------
toTable :: (Char -> Bool) -> (Char -> Bool)
toTable = inClass . toTableList
{-# INLINE toTable #-}


------------------------------------------------------------------------------
skipFieldChars :: Parser ()
skipFieldChars = skipWhile isFieldChar


------------------------------------------------------------------------------
isFieldChar :: Char -> Bool
isFieldChar = toTable f
  where
    f c = (isDigit c) || (isAlpha c) || c == '-' || c == '_'


------------------------------------------------------------------------------
-- | Parser for request headers.
pHeaders :: Parser [(ByteString, ByteString)]
pHeaders = many' header <?> "headers"
  where
    --------------------------------------------------------------------------
    slurp p = fst <$> match p

    --------------------------------------------------------------------------
    header            = {-# SCC "pHeaders/header" #-}
                        liftA2 (,)
                            fieldName
                            (char ':' *> skipSpace *> contents)

    --------------------------------------------------------------------------
    fieldName         = {-# SCC "pHeaders/fieldName" #-}
                        slurp (letter_ascii *> skipFieldChars)

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
                        skipWhile1 isLeadingWS

    --------------------------------------------------------------------------
    continuation      = {-# SCC "pHeaders/continuation" #-}
                        liftA2 S.cons
                               (leadingWhiteSpace *> pure ' ')
                               contents

    --------------------------------------------------------------------------
    skipWhile1 f = satisfy f *> skipWhile f


------------------------------------------------------------------------------
-- unhelpfully, the spec mentions "old-style" cookies that don't have quotes
-- around the value. wonderful.
pWord :: Parser ByteString
pWord = pWord' isRFCText


------------------------------------------------------------------------------
pWord' :: (Char -> Bool) -> Parser ByteString
pWord' charPred = pQuotedString' charPred <|> (takeWhile (/= ';'))


------------------------------------------------------------------------------
pQuotedString :: Parser ByteString
pQuotedString = pQuotedString' isRFCText


------------------------------------------------------------------------------
pQuotedString' :: (Char -> Bool) -> Parser ByteString
pQuotedString' charPred = q *> quotedText <* q
  where
    quotedText = (S.concat . L.toChunks . toLazyByteString) <$> f mempty

    f soFar = do
        t <- takeWhile qdtext
        let soFar' = soFar <> byteString t
        -- RFC says that backslash only escapes for <">
        choice [ string "\\\"" *> f (soFar' <> char8 '"')
               , pure soFar' ]

    q      = char '"'
    qdtext = matchAll [ charPred, (/= '"'), (/= '\\') ]


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
    b <- many' (skipSpace *> char ';' *> skipSpace *> pAvPair)
    return $! a:b


------------------------------------------------------------------------------
{-# INLINE pAvPair #-}
pAvPair :: Parser (ByteString, ByteString)
pAvPair = do
    key <- pToken <* skipSpace
    val <- liftM trim (option "" $ char '=' *> skipSpace *> pWord)
    return $! (key, val)


------------------------------------------------------------------------------
pParameter :: Parser (ByteString, ByteString)
pParameter = pParameter' isRFCText


------------------------------------------------------------------------------
pParameter' :: (Char -> Bool) -> Parser (ByteString, ByteString)
pParameter' valueCharPred = parser <?> "pParameter'"
  where
    parser = do
        key <- pToken <* skipSpace
        val <- liftM trim (char '=' *> skipSpace *> pWord' valueCharPred)
        return $! (trim key, val)


------------------------------------------------------------------------------
{-# INLINE trim #-}
trim :: ByteString -> ByteString
trim = snd . S.span isSpace . fst . S.spanEnd isSpace


------------------------------------------------------------------------------
pValueWithParameters :: Parser (ByteString, [(CI ByteString, ByteString)])
pValueWithParameters =  pValueWithParameters' isRFCText


------------------------------------------------------------------------------
pValueWithParameters' :: (Char -> Bool) -> Parser (ByteString, [(CI ByteString, ByteString)])
pValueWithParameters' valueCharPred = parser <?> "pValueWithParameters'"
  where
    parser = do
        value  <- liftM trim (skipSpace *> takeWhile (/= ';'))
        params <- many' pParam
        endOfInput
        return (value, map (first CI.mk) params)
    pParam = skipSpace *> char ';' *> skipSpace *> pParameter' valueCharPred


------------------------------------------------------------------------------
pContentTypeWithParameters :: Parser ( ByteString
                                     , [(CI ByteString, ByteString)] )
pContentTypeWithParameters = parser <?> "pContentTypeWithParameters"
  where
    parser = do
        value  <- liftM trim (skipSpace *> takeWhile (not . isSep))
        params <- many' (skipSpace *> satisfy isSep *> skipSpace *> pParameter)
        endOfInput
        return $! (value, map (first CI.mk) params)

    isSep c = c == ';' || c == ','


------------------------------------------------------------------------------
{-# INLINE pToken #-}
pToken :: Parser ByteString
pToken = takeWhile isToken


------------------------------------------------------------------------------
{-# INLINE isToken #-}
isToken :: Char -> Bool
isToken = toTable f
  where
    f = matchAll [ isAscii
                 , not . isControl
                 , not . isSpace
                 , not . flip elem [ '(', ')', '<', '>', '@', ',', ';'
                                   , ':', '\\', '\"', '/', '[', ']'
                                   , '?', '=', '{', '}' ]
                 ]


------------------------------------------------------------------------------
{-# INLINE pTokens #-}
-- | Used for "#field-name", and field-name = token, so "#token":
-- comma-separated tokens/field-names, like a header field list.
pTokens :: Parser [ByteString]
pTokens = (skipSpace *> pToken <* skipSpace) `sepBy'` char ','


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
             mzero

        let code = w2c ((unsafeFromHex hx) :: Word8)
        nextChunk $ l . ((S.singleton code) :)

    --------------------------------------------------------------------------
    unEncoded :: Char -> DList ByteString -> Parser (DList ByteString)
    unEncoded !c !l' = do
        let l = l' . ((S.singleton c) :)
        bs   <- takeTill (flip elem ['%', '+'])
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
-- | Decode an URL-escaped string (see
-- <http://tools.ietf.org/html/rfc2396.html#section-2.4>)
--
-- Example:
--
-- @
-- ghci> 'urlDecode' "1+attoparsec+%7e%3d+3+*+10%5e-2+meters"
-- Just "1 attoparsec ~= 3 * 10^-2 meters"
-- @
urlDecode :: ByteString -> Maybe ByteString
urlDecode = parseToCompletion pUrlEscaped
{-# INLINE urlDecode #-}


------------------------------------------------------------------------------
-- | URL-escape a string (see
-- <http://tools.ietf.org/html/rfc2396.html#section-2.4>)
--
-- Example:
--
-- @
-- ghci> 'urlEncode' "1 attoparsec ~= 3 * 10^-2 meters"
-- "1+attoparsec+%7e%3d+3+*+10%5e-2+meters"
-- @
urlEncode :: ByteString -> ByteString
urlEncode = S.concat . L.toChunks . toLazyByteString . urlEncodeBuilder
{-# INLINE urlEncode #-}


------------------------------------------------------------------------------
-- | URL-escape a string (see
-- <http://tools.ietf.org/html/rfc2396.html#section-2.4>) into a 'Builder'.
--
-- Example:
--
-- @
-- ghci> import "Data.ByteString.Builder"
-- ghci> 'toLazyByteString' . 'urlEncodeBuilder' $ "1 attoparsec ~= 3 * 10^-2 meters"
-- "1+attoparsec+%7e%3d+3+*+10%5e-2+meters"
-- @
urlEncodeBuilder :: ByteString -> Builder
urlEncodeBuilder = go mempty
  where
    go !b !s = maybe b' esc (S.uncons y)
      where
        (x,y)     = S.span urlEncodeClean s
        b'        = b <> byteString x
        esc (c,r) = let b'' = if c == ' '
                                then b' <> char8 '+'
                                else b' <> hexd c
                    in go b'' r


------------------------------------------------------------------------------
urlEncodeClean :: Char -> Bool
urlEncodeClean = toTable f
  where
    f c = any ($ c) [\c' -> isAscii c' && isAlphaNum c'
                    , flip elem [ '$', '_', '-', '.', '!'
                                , '*' , '\'', '(', ')', ',' ]]


------------------------------------------------------------------------------
hexd :: Char -> Builder
hexd c0 = char8 '%' <> word8 hi <> word8 low
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
-- | Parse a string encoded in @application/x-www-form-urlencoded@ < http://en.wikipedia.org/wiki/POST_%28HTTP%29#Use_for_submitting_web_forms format>.
--
-- Example:
--
-- @
-- ghci> 'parseUrlEncoded' "Name=John+Doe&Name=Jane+Doe&Age=23&Formula=a+%2B+b+%3D%3D+13%25%21"
-- 'Data.Map.fromList' [("Age",["23"]),("Formula",["a + b == 13%!"]),("Name",["John Doe","Jane Doe"])]
-- @
parseUrlEncoded :: ByteString -> Map ByteString [ByteString]
parseUrlEncoded s = foldr ins Map.empty decoded

  where
    --------------------------------------------------------------------------
    ins (!k,v) !m = Map.insertWith (++) k [v] m

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
-- | Like 'printUrlEncoded', but produces a 'Builder' instead of a
-- 'ByteString'. Useful for constructing a large string efficiently in
-- a single step.
--
-- Example:
--
-- @
-- ghci> import "Data.Map"
-- ghci> import "Data.Monoid"
-- ghci> import "Data.ByteString.Builder"
-- ghci> let bldr = 'buildUrlEncoded' ('Data.Map.fromList' [("Name", ["John Doe"]), ("Age", ["23"])])
-- ghci> 'toLazyByteString' $ 'byteString' "http://example.com/script?" <> bldr
-- "http://example.com/script?Age=23&Name=John+Doe"
-- @
buildUrlEncoded :: Map ByteString [ByteString] -> Builder
buildUrlEncoded m = mconcat builders
  where
    builders        = intersperse (char8 '&') $
                      concatMap encodeVS $ Map.toList m

    encodeVS (k,vs) = map (encodeOne k) vs

    encodeOne k v   = mconcat [ urlEncodeBuilder k
                              , char8 '='
                              , urlEncodeBuilder v ]


------------------------------------------------------------------------------
-- | Given a collection of key-value pairs with possibly duplicate
-- keys (represented as a 'Data.Map.Map'), construct a string in
-- @application/x-www-form-urlencoded@ format.
--
-- Example:
--
-- @
-- ghci> 'printUrlEncoded' ('Data.Map.fromList' [("Name", ["John Doe"]), ("Age", ["23"])])
-- "Age=23&Name=John+Doe"
-- @
printUrlEncoded :: Map ByteString [ByteString] -> ByteString
printUrlEncoded = S.concat . L.toChunks . toLazyByteString . buildUrlEncoded


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
-- Note: only works for nonnegative naturals
unsafeFromNat :: (Enum a, Num a, Bits a) => ByteString -> a
unsafeFromNat = S.foldl' f 0
  where
    zero = ord '0'
    f !cnt !i = cnt * 10 + toEnum (digitToInt i)

    digitToInt c = if d >= 0 && d <= 9
                     then d
                     else error $ "bad digit: '" ++ [c] ++ "'"
      where
        !d = ord c - zero
{-# INLINE unsafeFromNat #-}
