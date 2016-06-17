{-# language OverloadedStrings #-}
module Snap.Util.FileServe where

import           Snap.Core                      (modifyResponse,
                                                 MonadSnap,
                                                 setHeader,
                                                 emptyResponse,
                                                 setResponseCode,
                                                 setContentType,
                                                 liftSnap,
                                                 getRequest,
                                                 getHeader,
                                                 deleteHeader,
                                                 setContentLength)
import           Network.Wai                    (responseFile)
import           Data.ByteString                (ByteString)
import           Network.HTTP.Types.Status      (mkStatus)
import           Snap.Internal.Http.Server.Date (parseHttpTime,
                                                 formatHttpTime)
import           Control.Monad                  (liftM)
import           Control.Monad.Trans            (liftIO)
import           Data.Maybe                     (isNothing)


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

