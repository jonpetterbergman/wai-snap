{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module Snap.Util.FileServe where

import           Blaze.ByteString.Builder         (toByteString,
                                                   fromByteString,
                                                   fromWord8)
import           Blaze.ByteString.Builder.Char8   (fromShow)  
import           Snap.Core                        (modifyResponse,
                                                   MonadSnap,
                                                   setHeader,
                                                   emptyResponse,
                                                   setResponseCode,
                                                   setContentType,
                                                   liftSnap,
                                                   getRequest,
                                                   getHeader,
                                                   deleteHeader,
                                                   setContentLength,
                                                   setResponseBody,
                                                   finishWith,
                                                   sendFilePartial,
                                                   sendFile)
import           Network.Wai                      (Request,
                                                   FilePart(..))
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Internal         (c2w)
import           Data.Int                         (Int64)
import           Network.HTTP.Types.Status        (mkStatus)
import           Snap.Internal.Http.Server.Date   (parseHttpTime,
                                                   formatHttpTime)
import           Control.Monad                    (liftM,unless,when)
import           Control.Monad.Trans              (liftIO,MonadIO)
import           Data.Maybe                       (isNothing,fromMaybe)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict            as Map
import           System.FilePath                  (takeExtensions,
                                                   takeFileName)
import           System.PosixCompat.Files         (getFileStatus,
                                                   modificationTime,
                                                   fileSize)  
import           Snap.Internal.Debug              (debug)  
import           Data.Attoparsec.ByteString.Char8 (Parser,
                                                   char,
                                                   endOfInput,
                                                   option,
                                                   string)
import           Snap.Internal.Parsing            (parseNum,
                                                   fullyParse)
import           Control.Applicative              ((<|>))

dbg :: (MonadIO m) => String -> m ()
dbg s = debug $ "FileServe:" ++ s

lookupExt :: a -> HashMap FilePath a -> FilePath -> a
lookupExt def m f =
      if null ext
        then def
        else fromMaybe (lookupExt def m (drop 1 ext)) mbe
  where
    ext             = takeExtensions f
    mbe             = Map.lookup ext m

-- | A type alias for MIME type
type MimeMap = HashMap FilePath ByteString

fileType :: MimeMap -> FilePath -> ByteString
fileType = lookupExt defaultMimeType

-- | Serves a single file specified by a full or relative path.  If the file
-- does not exist, throws an exception (not that it does /not/ pass to the
-- next handler).   The path restrictions on 'serveDirectory' don't apply to
-- this function since the path is not being supplied by the user.
serveFile :: MonadSnap m
          => FilePath          -- ^ path to file
          -> m ()
serveFile fp = serveFileAs (fileType defaultMimeTypes (takeFileName fp)) fp

-- | Same as 'serveFile', with control over the MIME mapping used.
serveFileAs :: MonadSnap m
            => ByteString        -- ^ MIME type
            -> FilePath          -- ^ path to file
            -> m ()
serveFileAs mime fp = do
    reqOrig <- getRequest

    -- If-Range header must be ignored if there is no Range: header in the
    -- request (RFC 2616 section 14.27)
    let req = if isNothing $ getHeader "range" reqOrig
                then deleteHeader "if-range" reqOrig
                else reqOrig

    -- check "If-Modified-Since" and "If-Range" headers
    let mbH = getHeader "if-modified-since" req
    mbIfModified <- liftIO $ case mbH of
                               Nothing  -> return Nothing
                               (Just s) -> liftM Just $ parseHttpTime s

    -- If-Range header could contain an entity, but then parseHttpTime will
    -- fail and return 0 which means a 200 response will be generated anyways
    mbIfRange <- liftIO $ case getHeader "if-range" req of
                            Nothing  -> return Nothing
                            (Just s) -> liftM Just $ parseHttpTime s

    dbg $ "mbIfModified: " ++ Prelude.show mbIfModified
    dbg $ "mbIfRange: " ++ Prelude.show mbIfRange

    -- check modification time and bug out early if the file is not modified.
    --
    -- TODO: a stat cache would be nice here, but it'd need the date thread
    -- stuff from snap-server to be folded into snap-core
    filestat <- liftIO $ getFileStatus fp
    let mt = modificationTime filestat
    maybe (return $! ()) (\lt -> when (mt <= lt) notModified) mbIfModified

    let sz = fromIntegral $ fileSize filestat
    lm <- liftIO $ formatHttpTime mt

    -- ok, at this point we know the last-modified time and the
    -- content-type. set those.
    modifyResponse $ setHeader "Last-Modified" lm
                   . setHeader "Accept-Ranges" "bytes"
                   . setContentType mime


    -- now check: is this a range request? If there is an 'If-Range' header
    -- with an old modification time we skip this check and send a 200
    -- response
    let skipRangeCheck = maybe (False)
                               (\lt -> mt > lt)
                               mbIfRange

    -- checkRangeReq checks for a Range: header in the request and sends a
    -- partial response if it matches.
    wasRange <- if skipRangeCheck
                  then return False
                  else liftSnap $ checkRangeReq req fp sz

    dbg $ "was this a range request? " ++ Prelude.show wasRange

    -- if we didn't have a range request, we just do normal sendfile
    unless wasRange $ do
      modifyResponse $ setResponseCode 200
                     . setContentLength sz
      liftSnap $ sendFile fp

  where
    --------------------------------------------------------------------------
    notModified = finishWith $
                  setResponseCode 304 emptyResponse


defaultMimeTypes :: MimeMap
defaultMimeTypes = Map.fromList [
  ( ".asc"     , "text/plain"                        ),
  ( ".asf"     , "video/x-ms-asf"                    ),
  ( ".asx"     , "video/x-ms-asf"                    ),
  ( ".avi"     , "video/x-msvideo"                   ),
  ( ".bz2"     , "application/x-bzip"                ),
  ( ".c"       , "text/plain"                        ),
  ( ".class"   , "application/octet-stream"          ),
  ( ".conf"    , "text/plain"                        ),
  ( ".cpp"     , "text/plain"                        ),
  ( ".css"     , "text/css"                          ),
  ( ".cxx"     , "text/plain"                        ),
  ( ".dtd"     , "text/xml"                          ),
  ( ".dvi"     , "application/x-dvi"                 ),
  ( ".gif"     , "image/gif"                         ),
  ( ".gz"      , "application/x-gzip"                ),
  ( ".hs"      , "text/plain"                        ),
  ( ".htm"     , "text/html"                         ),
  ( ".html"    , "text/html"                         ),
  ( ".ico"     , "image/x-icon"                      ),
  ( ".jar"     , "application/x-java-archive"        ),
  ( ".jpeg"    , "image/jpeg"                        ),
  ( ".jpg"     , "image/jpeg"                        ),
  ( ".js"      , "text/javascript"                   ),
  ( ".json"    , "application/json"                  ),
  ( ".log"     , "text/plain"                        ),
  ( ".m3u"     , "audio/x-mpegurl"                   ),
  ( ".mov"     , "video/quicktime"                   ),
  ( ".mp3"     , "audio/mpeg"                        ),
  ( ".mpeg"    , "video/mpeg"                        ),
  ( ".mpg"     , "video/mpeg"                        ),
  ( ".ogg"     , "application/ogg"                   ),
  ( ".pac"     , "application/x-ns-proxy-autoconfig" ),
  ( ".pdf"     , "application/pdf"                   ),
  ( ".png"     , "image/png"                         ),
  ( ".ps"      , "application/postscript"            ),
  ( ".qt"      , "video/quicktime"                   ),
  ( ".sig"     , "application/pgp-signature"         ),
  ( ".spl"     , "application/futuresplash"          ),
  ( ".svg"     , "image/svg+xml"                     ),
  ( ".swf"     , "application/x-shockwave-flash"     ),
  ( ".tar"     , "application/x-tar"                 ),
  ( ".tar.bz2" , "application/x-bzip-compressed-tar" ),
  ( ".tar.gz"  , "application/x-tgz"                 ),
  ( ".tbz"     , "application/x-bzip-compressed-tar" ),
  ( ".text"    , "text/plain"                        ),
  ( ".tgz"     , "application/x-tgz"                 ),
  ( ".torrent" , "application/x-bittorrent"          ),
  ( ".ttf"     , "application/x-font-truetype"       ),
  ( ".txt"     , "text/plain"                        ),
  ( ".wav"     , "audio/x-wav"                       ),
  ( ".wax"     , "audio/x-ms-wax"                    ),
  ( ".wma"     , "audio/x-ms-wma"                    ),
  ( ".wmv"     , "video/x-ms-wmv"                    ),
  ( ".xbm"     , "image/x-xbitmap"                   ),
  ( ".xml"     , "text/xml"                          ),
  ( ".xpm"     , "image/x-xpixmap"                   ),
  ( ".xwd"     , "image/x-xwindowdump"               ),
  ( ".zip"     , "application/zip"                   ) ]
                   
defaultMimeType :: ByteString                   
defaultMimeType = "application/octet-stream"

------------------------------------------------------------------------------
checkRangeReq :: (MonadSnap m) => Request -> FilePath -> Int64 -> m Bool
checkRangeReq req fp sz = do
    -- TODO/FIXME: multiple ranges
    dbg $ "checkRangeReq, fp=" ++ fp ++ ", sz=" ++ Prelude.show sz
    maybe (return False)
          (\s -> either (const $ return False)
                        withRange
                        (fullyParse s rangeParser))
          (getHeader "range" req)

  where
    withRange rng@(RangeReq start mend) = do
        dbg $ "withRange: got Range request: " ++ Prelude.show rng
        let end = fromMaybe (sz-1) mend
        dbg $ "withRange: start=" ++ Prelude.show start
                  ++ ", end=" ++ Prelude.show end

        if start < 0 || end < start || start >= sz || end >= sz
           then send416
           else send206 start end

    withRange rng@(SuffixRangeReq nbytes) = do
        dbg $ "withRange: got Range request: " ++ Prelude.show rng
        let end   = sz-1
        let start = sz - nbytes

        dbg $ "withRange: start=" ++ Prelude.show start
                  ++ ", end=" ++ Prelude.show end

        if start < 0 || end < start || start >= sz || end >= sz
           then send416
           else send206 start end

    -- note: start and end INCLUSIVE here
    send206 start end = do
        dbg "inside send206"
        let len = end-start+1
        let crng = toByteString $
                   mconcat [ fromByteString "bytes "
                           , fromShow start
                           , fromWord8 (c2w '-')
                           , fromShow end
                           , fromWord8 (c2w '/')
                           , fromShow sz ]

        modifyResponse $ setResponseCode 206
                       . setHeader "Content-Range" crng
                       . setContentLength len

        dbg $ "send206: sending range (" ++ Prelude.show start
                ++ "," ++ Prelude.show (end+1) ++ ") to sendFilePartial"

        -- end here was inclusive, sendFilePartial is exclusive
        sendFilePartial fp $ Just $ FilePart (toInteger start) (toInteger len) (toInteger sz)
        return True


    send416 = do
        dbg "inside send416"
        -- if there's an "If-Range" header in the request, then we just send
        -- back 200
        if getHeader "If-Range" req /= Nothing
           then return False
           else do
               let crng = toByteString $
                          mconcat [ fromByteString "bytes */"
                                  , fromShow sz ]

               modifyResponse $ setResponseCode 416
                              . setHeader "Content-Range" crng
                              . setContentLength 0
                              . deleteHeader "Content-Type"
                              . deleteHeader "Content-Encoding"
                              . deleteHeader "Transfer-Encoding"
                              . setResponseBody mempty

               return True
               
------------------------------------------------------------------------------               
data RangeReq = RangeReq { _rangeFirst :: !Int64
                         , _rangeLast  :: !(Maybe Int64)
                         }
              | SuffixRangeReq { _suffixLength :: !Int64 }
              deriving (Eq, Prelude.Show)

------------------------------------------------------------------------------
rangeParser :: Parser RangeReq
rangeParser = string "bytes=" *>
              (byteRangeSpec <|> suffixByteRangeSpec) <*
              endOfInput
  where
    byteRangeSpec = do
        start <- parseNum
        char '-'
        end   <- option Nothing $ liftM Just parseNum

        return $! RangeReq start end

    suffixByteRangeSpec = liftM SuffixRangeReq $ char '-' *> parseNum
