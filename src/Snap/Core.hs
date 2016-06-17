{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
module Snap.Core where

import           Blaze.ByteString.Builder     (Builder,fromByteString,toByteString)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8      as Char8

import           Control.Monad.State          (StateT,runStateT,get,modify)
import           Control.Monad.Except         (MonadError(..))
import           Control.Monad.IO.Class       (MonadIO(..))
import           Control.Monad                (ap,liftM,MonadPlus(..))
import           Control.Applicative          (Alternative(..))
import           Data.CaseInsensitive         (CI)  
import           Data.HashMap.Strict          (HashMap)   
import qualified Data.HashMap.Strict        as HashMap
import           Data.Map                     (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                   (listToMaybe)
import           Data.Time                    (UTCTime)
import           Data.Int                     (Int64)
import qualified Data.List                  as List
import           Data.Text.Encoding           (encodeUtf8)

import           Network.Wai                  (Application)
import           Network.Wai.Internal         (Response(..),Request(..))
import qualified Network.Wai               as  Wai
import           Network.Socket               (SockAddr(..))
import           Network.HTTP.Types           (hCookie,decodePathSegments)
import           Network.HTTP.Types.Status    (mkStatus) 
import           Network.HTTP.Types.Header    (Header)
import           Web.Cookie                   (Cookies,parseCookies,SetCookie,renderSetCookie)

type Cookie = SetCookie

-- | Fails out of a 'Snap' monad action.  This is used to indicate
-- that you choose not to handle the given request within the given
-- handler.
pass :: MonadSnap m => m a
pass = empty

type Headers = [Header]

class HasHeaders a where
      -- | Modify the datatype's headers.
      updateHeaders :: (Headers -> Headers) -> a -> a
      
      -- | Retrieve the headers from a datatype that has headers.
      headers       :: a -> Headers

instance HasHeaders Request where
  updateHeaders f r = r { requestHeaders = f (requestHeaders r) }
  headers = requestHeaders
  

newtype Snap a = Snap {
  unSnap :: StateT SnapState IO (SnapResult a)
  }
                 
-- | 'MonadSnap' is a type class, analogous to 'MonadIO' for 'IO', that makes                 
-- it easy to wrap 'Snap' inside monad transformers.
class (Monad m, MonadIO m, MonadPlus m, Functor m,
       Applicative m, Alternative m) => MonadSnap m where
  liftSnap :: Snap a -> m a
                 
data SnapResult a = SnapValue a
                  | PassOnProcessing String
                  | EarlyTermination Response

data SnapState = SnapState                 
  { _snapRequest :: Request
  , _snapResponse :: Response
  , _snapLogError :: ByteString -> IO ()
  , _snapModifyTimeout :: (Int -> Int) -> IO ()
  }
  
instance Applicative Snap where
   pure  = return
   (<*>) = ap

instance Monad Snap where  
   (>>=)  = snapBind
   return = snapReturn
   fail   = snapFail
   
instance MonadIO Snap where   
   liftIO m = Snap $! liftM SnapValue $! liftIO m
   
instance MonadSnap Snap where
   liftSnap = id

instance (MonadError String) Snap where
   throwError = fail
   catchError act hndl = Snap $ do
     r <- unSnap act
     case r of
       SnapValue _        -> return r
       PassOnProcessing m -> unSnap $ hndl m
       _                  -> return r


instance Functor Snap where   
   fmap = liftM

instance MonadPlus Snap where
   mzero = Snap $! return $! PassOnProcessing ""
   a `mplus` b =
       Snap $! do
           r <- unSnap a
           case r of
             SnapValue _        -> return r
             PassOnProcessing _ -> unSnap b
             _                  -> return r

instance Alternative Snap where
   empty = mzero
   (<|>) = mplus

snapBind :: Snap a -> (a -> Snap b) -> Snap b  
snapBind (Snap m) f = Snap $ do
      res <- m
      case res of
        SnapValue a        -> unSnap $! f a
        PassOnProcessing r -> return $! PassOnProcessing r
        EarlyTermination r -> return $! EarlyTermination r
  
snapReturn :: a -> Snap a
snapReturn = Snap . return . SnapValue

snapFail :: String -> Snap a
snapFail !m = Snap $! return $! PassOnProcessing m

snapToWai :: Snap a -> Int -> Application  
snapToWai (Snap m) serverPort req aCont = do
    (r,ss') <- runStateT m $ SnapState req dresp dummyLog (const $ return ())
    let resp = case r of
                 SnapValue _ -> _snapResponse ss'
                 PassOnProcessing _ -> fourohfour req
                 EarlyTermination x -> x
    let req' = _snapRequest ss'
    aCont resp
  where dresp = emptyResponse

emptyResponse :: Response
emptyResponse = 
  ResponseBuilder (mkStatus 200 "OK") [] mempty
        
fourohfour :: Request -> Response        
fourohfour req = setResponseStatus 404 "Not Found" $ 
                 setResponseBody (body404 req) $ 
                 emptyResponse        
        
rqURI :: Request -> ByteString             
rqURI = rawPathInfo
             
body404 :: Request -> Builder             
body404 req = mconcat $ map fromByteString html
  where html = [ Char8.concat [ "<!DOCTYPE html>\n"
                              , "<html>\n"
                              , "<head>\n"
                              , "<title>Not found</title>\n"
                              , "</head>\n"
                              , "<body>\n"
                              , "<code>No handler accepted \""
                              ]
               , rqURI req
               , "\"</code>\n</body></html>"
               ]
             
-- | Sets an HTTP response body to the given 'Builder' value.             
setResponseBody     :: Builder  -- ^ new response body builder
                    -> Response -- ^ response to modify
                    -> Response
setResponseBody b (ResponseFile s h _ _) = ResponseBuilder s h b
setResponseBody b (ResponseBuilder s h _) = ResponseBuilder s h b
setResponseBody b (ResponseStream s h _) = ResponseBuilder s h b
setResponseBody b _ = ResponseBuilder (mkStatus 200 "OK") [] b

instance HasHeaders Response where
  updateHeaders f (ResponseFile s h fi fp) = ResponseFile s (f h) fi fp
  updateHeaders f (ResponseBuilder s h b) = ResponseBuilder s (f h) b
  updateHeaders f (ResponseStream s h st) = ResponseStream s (f h) st
  updateHeaders _ r = r
  headers (ResponseFile _ h _ _) = h
  headers (ResponseBuilder _ h _) = h
  headers (ResponseStream _ h _ ) = h
  headers _ = []


-- | Sets the HTTP response status. Note: normally you would use
-- 'setResponseCode' unless you needed a custom response explanation.
setResponseStatus   :: Int        -- ^ HTTP response integer code
                    -> ByteString -- ^ HTTP response explanation
                    -> Response   -- ^ Response to be modified
                    -> Response
setResponseStatus s reason (ResponseFile _ h f fp) = ResponseFile (mkStatus s reason) h f fp
setResponseStatus s reason (ResponseBuilder _ h b) = ResponseBuilder (mkStatus s reason) h b
setResponseStatus s reason (ResponseStream _ h b) = ResponseStream (mkStatus s reason) h b
setResponseStatus s reason _ = ResponseBuilder (mkStatus s reason) [] mempty

dummyLog :: ByteString -> IO ()        
dummyLog = Char8.putStrLn
        
-- | Grabs the 'Request' object out of the 'Snap' monad.  
getRequest :: MonadSnap m => m Request
getRequest = liftSnap $ liftM _snapRequest sget


-- | Grabs something out of the 'Request' object, using the given projection
-- function. See 'gets'.
getsRequest :: MonadSnap m => (Request -> a) -> m a
getsRequest f = liftSnap $ liftM (f . _snapRequest) sget

-- | Local Snap version of 'get'.
sget :: Snap SnapState
sget = Snap $ liftM SnapValue get

-- | Puts a new 'Request' object into the 'Snap' monad.
putRequest :: MonadSnap m => Request -> m ()
putRequest r = liftSnap $ smodify $ \ss -> ss { _snapRequest = r }

-- | Local Snap monad version of 'modify'.
smodify :: (SnapState -> SnapState) -> Snap ()
smodify f = Snap $ modify f >> return (SnapValue ())

-- | Gets a header value out of a 'HasHeaders' datatype. If many headers came
-- in with the same name, they will be catenated together.
getHeader :: (HasHeaders a) => CI ByteString -> a -> Maybe ByteString
getHeader k a = 
  case map snd $ List.filter ((== k) . fst) $ headers a of
    [] -> Nothing
    xs -> Just $ Char8.intercalate "," xs
    
-- | Sets a header key-value-pair in a 'HasHeaders' datatype. If a header with                
-- the same name already exists, it is overwritten with the new value.
setHeader :: (HasHeaders a) => CI ByteString -> ByteString -> a -> a
setHeader k v = updateHeaders (((k,v):) . List.filter ((/= k) . fst))
    
rqPathInfo :: Request -> ByteString
rqPathInfo = encodeUtf8 . mconcat . pathInfo

setRqPathInfo :: ByteString -> Request -> Request
setRqPathInfo b r = r { pathInfo = decodePathSegments b }

-- | Modifes the 'Response' object stored in a 'Snap' monad.
modifyResponse :: MonadSnap m => (Response -> Response) -> m ()
modifyResponse f = liftSnap $
   smodify $ \ss -> ss { _snapResponse = f $ _snapResponse ss }
                    
-- | Gets the HTTP 'Cookie' with the specified name.                    
getCookie :: MonadSnap m
          => ByteString
          -> m (Maybe ByteString)
getCookie name = withRequest $
  return . List.lookup name . rqCookies
  
-- | Fetches the 'Request' from state and hands it to the given action.  
withRequest :: MonadSnap m => (Request -> m a) -> m a
withRequest = (getRequest >>=)
  
rqCookies :: Request -> Cookies
rqCookies = maybe [] parseCookies . List.lookup hCookie . headers

-- | Adds an HTTP 'Cookie' to 'Response' headers.
addResponseCookie :: Cookie            -- ^ cookie value
                  -> Response          -- ^ response to modify
                  -> Response
addResponseCookie c = setHeader hCookie $ toByteString $ renderSetCookie c
