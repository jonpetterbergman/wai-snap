{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Language handling for Snap.
--
-- Support for determining the client's prefered language using
-- the Accept-Language header or using suffixes to the requested URI.

module Snap.Language 
  ( RangeMapping
  , getAcceptLanguage
  , getSuffixLanguage
  , switchSuffixLanguage
  , setContentLanguage
  ) where

import           Data.Attoparsec.ByteString.Char8(parseOnly,
                                                  string,
                                                  double,
                                                  Parser,
                                                  letter_ascii,
                                                  many1,
                                                  many',
                                                  char,
                                                  option,
                                                  eitherP,
                                                  sepBy,
                                                  skipSpace,
                                                  endOfLine)
import           Data.ByteString                 (ByteString,
                                                  isSuffixOf)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.Char                       (toLower)
import           Data.List                       (intersperse,isPrefixOf,find)
import           Control.Applicative             ((*>),(<$>),(<*>),(<|>))
import           Snap.Core                       (getsRequest,
                                                  getRequest,
                                                  putRequest,
                                                  getHeader,
                                                  MonadSnap,
                                                  Cookie(..),
                                                  addResponseCookie,
                                                  modifyResponse,
                                                  getCookie,
                                                  setHeader,
                                                  pass,
                                                  rqPathInfo)
import           Data.Maybe                      (mapMaybe,
                                                  listToMaybe)
import           Data.Map                        (Map,
                                                  toList)
import           Data.Tuple                      (swap)

range :: Parser String
range = (++) <$> mletters <*> (fmap concat $ many' $ (:) <$> (char '-') <*> mletters)
  where mletters = many1 letter_ascii

rangeval :: Parser (Maybe String, Double)
rangeval = 
  do
    r <- eitherP (char '*') range
    q <- option 1 $ string ";q=" *> double
    return (either (const Nothing) Just r,q)

acceptLanguageParser :: Parser [(Maybe String, Double)]
acceptLanguageParser = skipSpace *> (sepBy rangeval $ skipSpace *> char ',' <* skipSpace)

matches :: String 
        -> Maybe String 
        -> Bool
matches _ Nothing = True
matches provided (Just requested) = 
  (map toLower requested) `isPrefixOf` (map toLower provided)

candidates :: Map String a
           -> [(Maybe String, Double)]
           -> [(a,Double)]
candidates provided requested = concatMap go $ toList provided
  where go (range,x) = map (\(a,b) -> (x,b)) $ filter (matches range . fst) requested

pickLanguage' :: Map String a
              -> [(Maybe String,Double)]
              -> Maybe a
pickLanguage' provided requested = fmap fst $ foldr go Nothing $ candidates provided requested
  where go r'           Nothing                      = return r'
        go r'@(val',q') (Just r@(val,q)) | q' > q    = return r'
                                         | otherwise = return r 

pickLanguage :: Map String a
             -> ByteString
             -> Maybe a
pickLanguage provided headerString = 
  either (const Nothing) (pickLanguage' provided) $ parseOnly acceptLanguageParser headerString

-- | Attempt to find a suitable language according to the Accept-Language
-- header of the request. 
--
-- This handler will call pass if it cannot find a suitable language.
getAcceptLanguage :: MonadSnap m
                   => RangeMapping a
                   -> m a
getAcceptLanguage rangeMapping =
  do
    al <- getsRequest $ getHeader "Accept-Language"
    maybe pass return $ al >>= pickLanguage rangeMapping

-- | A mapping from language ranges as defined in rfc2616 to languages in your own representation.
-- 
-- For example:
--
-- > data Language = EN | SV deriving Eq
-- > 
-- > mapping :: RangeMapping Language
-- > mapping = Map.fromList [("en-gb",EN),("sv-se",SV)]
type RangeMapping a = Map String a

removeSuffix :: ByteString
             -> ByteString
             -> Maybe ByteString
removeSuffix suf x | suf `B.isSuffixOf` x = Just $ B.take ((B.length x) - (B.length suf)) x
                   | otherwise            = Nothing

suffixes :: RangeMapping a
         -> [(ByteString,a)]
suffixes = map go . toList
  where go (str,val) = (BC.pack $ '.':str,val)

matchSuffix :: ByteString
            -> [(ByteString,a)]
            -> Maybe (ByteString,a)
matchSuffix str sfxs = listToMaybe $ mapMaybe go sfxs
  where go (sfx,val) = fmap (,val) $ removeSuffix sfx str

-- | Attempt to find a suitable language according to a suffix in the request URI corresponding to a language range.
--
-- Will call pass if it cannot find a suitable language.
--
-- If a match is found, the suffix will be removed from the URI in the request, so that you
-- can later match on your resource as usual and not worry about suffixes.
--
-- For example, with the following requested URI:
-- 
-- > /resource.en-gb?param=value
--
-- 'getSuffixLanguage' with our previously defined mapping will return 'EN' and 'rqPathInfo' will be changed to:
--
-- > /resource?param=value
getSuffixLanguage :: MonadSnap m
                  => RangeMapping a
                  -> m a
getSuffixLanguage rangeMapping = 
  do
    r <- getRequest
    case matchSuffix (rqPathInfo r) $ suffixes rangeMapping of
      Nothing -> pass
      Just (rqPathInfo',val) -> 
        do
          putRequest $ r { rqPathInfo = rqPathInfo' }
          return val

-- | Change, or remove, the language suffix of an URI.
switchSuffixLanguage :: Eq a
                     => RangeMapping a
                     -> ByteString -- ^ The URI.
                     -> Maybe a    -- ^ The language to be appended to the URI, or Nothing to remove language suffix.
                     -> ByteString
switchSuffixLanguage rangeMapping uri lang = maybe (addSuffix lang path) (addSuffix lang . fst) $ matchSuffix path $ suffixes rangeMapping
  where (path,params)    = BC.break ((==) '?') uri
        addSuffix lang p = B.concat [p,findSfx lang,params]
        findSfx Nothing  = B.empty
        findSfx (Just l) = maybe B.empty id $ lookup l $ map swap $ suffixes rangeMapping

-- | Set the Content-Language header in the response.
setContentLanguage :: (Eq a, MonadSnap m)
                   => RangeMapping a
                   -> a
                   -> m ()
setContentLanguage rangeMapping val =
 maybe (return ()) go $ lookup val $ map swap $ toList rangeMapping
   where go = modifyResponse . setHeader "Content-Language" . BC.pack

