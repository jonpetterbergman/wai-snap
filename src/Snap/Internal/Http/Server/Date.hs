{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Snap.Internal.Http.Server.Date
( getDateString
, getLogDateString
, getCurrentDateTime) where

import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import           Data.ByteString.Internal (c2w,w2c)
import           Data.IORef
import           Foreign.C.Types
import           System.IO.Unsafe
import           System.PosixCompat.Time

import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Time.Clock.POSIX

import           Data.Time(UTCTime)

-- local definitions
fromStr :: String -> ByteString
fromStr = S.pack . map c2w
{-# INLINE fromStr #-}

------------------------------------------------------------------------------
-- private helper functions
toStr :: ByteString -> String
toStr = map w2c . S.unpack

-- | Converts a 'CTime' into an HTTP timestamp.
formatHttpTime :: CTime -> IO ByteString

-- | Converts a 'CTime' into common log entry format.
formatLogTime :: CTime -> IO ByteString

------------------------------------------------------------------------------
formatHttpTime = return . format . toUTCTime
  where
    format :: UTCTime -> ByteString
    format = fromStr . formatTime defaultTimeLocale "%a, %d %b %Y %X GMT"

    toUTCTime :: CTime -> UTCTime
    toUTCTime = posixSecondsToUTCTime . realToFrac


------------------------------------------------------------------------------
formatLogTime ctime = do
  t <- utcToLocalZonedTime $ toUTCTime ctime
  return $! format t
    
  where
    format :: ZonedTime -> ByteString
    format = fromStr . formatTime defaultTimeLocale "%d/%b/%Y:%H:%M:%S %z"
                         
    toUTCTime :: CTime -> UTCTime
    toUTCTime = posixSecondsToUTCTime . realToFrac


------------------------------------------------------------------------------
parseHttpTime = return . toCTime . prs . toStr
  where
    prs :: String -> Maybe UTCTime
    prs = parseTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"

    toCTime :: Maybe UTCTime -> CTime
    toCTime (Just t) = fromInteger $ truncate $ utcTimeToPOSIXSeconds t
    toCTime Nothing  = fromInteger 0


------------------------------------------------------------------------------
data DateState = DateState {
      _cachedDateString :: !(IORef ByteString)
    , _cachedLogString  :: !(IORef ByteString)
    , _lastFetchTime    :: !(IORef CTime)
    }


------------------------------------------------------------------------------
dateState :: DateState
dateState = unsafePerformIO $ do
    (s1,s2,date) <- fetchTime
    bs1 <- newIORef s1
    bs2 <- newIORef s2
    dt  <- newIORef date

    return $! DateState bs1 bs2 dt


------------------------------------------------------------------------------
fetchTime :: IO (ByteString,ByteString,CTime)
fetchTime = do
    now <- epochTime
    t1  <- formatHttpTime now
    t2  <- formatLogTime now
    return (t1, t2, now)


------------------------------------------------------------------------------
updateState :: DateState -> IO ()
updateState (DateState dateString logString time) = do
    (s1,s2,now) <- fetchTime
    atomicModifyIORef dateString $ const (s1,())
    atomicModifyIORef logString  $ const (s2,())
    atomicModifyIORef time       $ const (now,())

    -- force values in the iorefs to prevent thunk buildup
    !_ <- readIORef dateString
    !_ <- readIORef logString
    !_ <- readIORef time

    return ()


------------------------------------------------------------------------------
ensureFreshDate :: IO ()
ensureFreshDate = mask $ \_ -> do
    now <- epochTime
    old <- readIORef $ _lastFetchTime dateState
    when (now > old) $ updateState dateState


------------------------------------------------------------------------------
getDateString :: IO ByteString
getDateString = mask $ \_ -> do
    ensureFreshDate
    readIORef $ _cachedDateString dateState


------------------------------------------------------------------------------
getLogDateString :: IO ByteString
getLogDateString = mask $ \_ -> do
    ensureFreshDate
    readIORef $ _cachedLogString dateState


------------------------------------------------------------------------------
getCurrentDateTime :: IO CTime
getCurrentDateTime = epochTime
