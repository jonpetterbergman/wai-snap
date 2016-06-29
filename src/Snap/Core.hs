{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language ScopedTypeVariables#-}
{-# language RankNTypes #-}
module Snap.Core where

import           Blaze.ByteString.Builder     (Builder,
                                               fromByteString,
                                               toByteString,
                                               fromLazyByteString)
import           Blaze.ByteString.Builder.Char.Utf8(fromLazyText)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Char8      as Char8
import           Data.ByteString.Internal     (c2w)
import qualified Data.HashMap.Strict as H

import           Control.Monad.Cont           (ContT(..))
import           Control.Monad.Except         (ExceptT(..),Except(..))
import           Control.Monad.Reader         (ReaderT(..))
import           Control.Monad.Writer.Strict  (WriterT(..))
import           Control.Monad.State.Strict   (StateT,runStateT,get,modify,execStateT)
import           Control.Monad.Except         (MonadError(..))
import           Control.Monad.IO.Class       (MonadIO(..))
import           Control.Monad.List           (ListT(..))
import           Control.Monad.RWS.Strict    hiding (pass)
import qualified Control.Monad.RWS.Lazy     as LRWS
import qualified Control.Monad.State.Lazy   as LState
import qualified Control.Monad.Writer.Lazy  as LWriter
import           Control.Monad                (ap,liftM,MonadPlus(..),unless,join)
import           Control.Monad.Trans          (lift)
import           Control.Monad.Trans.Control  (MonadBaseControl(..),
                                               MonadTransControl(..),
                                               defaultLiftWith,
                                               defaultRestoreT,
                                               RunInBase)
import           Control.Monad.Base           (MonadBase(..))
import           Control.Applicative          (Alternative(..))
import           Data.CaseInsensitive         (CI)  
import           Data.HashMap.Strict          (HashMap)   
import qualified Data.HashMap.Strict        as HashMap
import           Data.Map                     (Map)
import qualified Data.Map                   as Map
import qualified Data.IntMap                as IM
import           Data.Maybe                   (listToMaybe,fromMaybe)
import           Data.Time                    (UTCTime(..),Day(..))
import           Data.Int                     (Int64)
import qualified Data.List                  as List
import qualified Data.Text.Lazy             as LT
import           Data.Default                 (def)
import           Data.Text.Encoding           (encodeUtf8)
import           Data.Function                (on)

import           Network.Wai                  (Application,FilePart,requestMethod)
import           Network.Wai.Internal         (Response(..),Request(..))
import qualified Network.Wai               as  Wai
import           Network.Socket               (SockAddr(..))
import           Network.HTTP.Types           (hCookie,
                                               decodePathSegments,
                                               encodePathSegments,
                                               hContentType,
                                               hContentLength,
                                               Query)
import           Network.HTTP.Types.Status    (mkStatus) 
import           Network.HTTP.Types.Header    (Header)
import           Network.HTTP.Types.Method    (StdMethod(..),parseMethod)
import           Web.Cookie                   (Cookies,parseCookies,SetCookie(..),renderSetCookie)
import           Snap.Internal.Parsing        (urlDecode)

type Method = Either ByteString StdMethod

type Cookie = SetCookie

-- | Fails out of a 'Snap' monad action.  This is used to indicate
-- that you choose not to handle the given request within the given
-- handler.
pass :: MonadSnap m => m a
pass = empty

-- | Short-circuits a 'Snap' monad action early, storing the given
-- 'Response' value in its state.
finishWith :: MonadSnap m => Response -> m a
finishWith = liftSnap . Snap . return . EarlyTermination

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
                 
instance MonadPlus m => MonadPlus (ContT c m) where
    mzero = lift mzero
    m `mplus` n = ContT $ \ f -> runContT m f `mplus` runContT n f

instance MonadPlus m => Alternative (ContT c m) where
    empty = mzero
    (<|>) = mplus

instance MonadSnap m => MonadSnap (ContT c m) where
    liftSnap = lift . liftSnap

instance (MonadSnap m,Monoid e) => MonadSnap (ExceptT e m) where
    liftSnap = lift . liftSnap

instance MonadSnap m => MonadSnap (ListT m) where
    liftSnap = lift . liftSnap

instance (MonadSnap m, Monoid w) => MonadSnap (RWST r w s m) where
    liftSnap = lift . liftSnap

instance (MonadSnap m, Monoid w) => MonadSnap (LRWS.RWST r w s m) where
    liftSnap = lift . liftSnap

instance MonadSnap m => MonadSnap (ReaderT r m) where
    liftSnap = lift . liftSnap

instance MonadSnap m => MonadSnap (StateT s m) where
    liftSnap = lift . liftSnap

instance MonadSnap m => MonadSnap (LState.StateT s m) where
    liftSnap = lift . liftSnap

instance (MonadSnap m, Monoid w) => MonadSnap (WriterT w m) where
    liftSnap = lift . liftSnap

instance (MonadSnap m, Monoid w) => MonadSnap (LWriter.WriterT w m) where
    liftSnap = lift . liftSnap
              
instance MonadBase IO Snap where
  liftBase act = liftIO $ liftBase act             
  
instance MonadBaseControl IO Snap where  
  type StM Snap a = (SnapResult a,SnapState)
  liftBaseWith f = 
    Snap $ fmap SnapValue $ liftBaseWith $ \rib -> f (rib . unSnap)
  restoreM = Snap . restoreM
  
--instance MonadBaseControl IO Snap where  
--  type StM Snap a = (a,SnapState)
--  liftBaseWith f = 
--    Snap $ fmap SnapValue $ liftBaseWith $ \rib -> f ((\x -> rib x >>= \(SnapValue x,y) -> return (x,y)) . unSnap)
--  restoreM (x,s) = Snap $ restoreM (SnapValue x,s)
          
--instance MonadBaseControl IO Snap where 
--  type StM Snap a = (SnapResult a,SnapState)
--  liftBaseWith f = do s <- sget
--                      liftIO $ f (flip runStateT s . unSnap)
--  restoreM (x,s) = Snap $ (put s >> return x)    
              
data SnapResult a = SnapValue a
                  | PassOnProcessing String
                  | EarlyTermination Response

data SnapState = SnapState                 
  { _snapRequest :: Request
  , _snapResponse :: Response
  , _snapLogError :: ByteString -> IO ()
  , _snapModifyTimeout :: (Int -> Int) -> IO ()
  , _snapPort :: Int
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
    (r,ss') <- runStateT m $ SnapState req dresp dummyLog (const $ return ()) serverPort
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

getResponseBody :: Response -> Builder
getResponseBody (ResponseBuilder _ _ b) = b
getResponseBody _ = mempty

modifyResponseBody :: (Builder -> Builder) -> Response -> Response
modifyResponseBody f r = setResponseBody (f $ getResponseBody r) r

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

-- | Sets the @Content-Type@ in the 'Response' headers.
setContentType      :: ByteString -> Response -> Response
setContentType = setHeader hContentType

-- | Sets content length, in the headers
setContentLength :: Int64 -> Response -> Response
setContentLength n = setHeader hContentLength (Char8.pack $ show n)

-- | Sets the HTTP response code.
setResponseCode   :: Int        -- ^ HTTP response integer code
                  -> Response   -- ^ Response to be modified
                  -> Response
setResponseCode s r = setResponseStatus s reason r
  where
    reason = fromMaybe "Unknown" (IM.lookup s statusReasonMap)

dummyLog :: ByteString -> IO ()        
dummyLog = Char8.putStrLn
        
-- | Grabs the 'Request' object out of the 'Snap' monad.  
getRequest :: MonadSnap m => m Request
getRequest = liftSnap $ liftM _snapRequest sget

getResponse :: MonadSnap m => m Response
getResponse = liftSnap $ liftM _snapResponse sget

getServerPort :: MonadSnap m => m Int
getServerPort = liftSnap $ liftM _snapPort sget

rqServerName :: Request -> Maybe ByteString
rqServerName = fmap (Char8.takeWhile (/= ':')) . requestHeaderHost

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
    
-- | Clears a header value from a 'HasHeaders' datatype.
deleteHeader :: (HasHeaders a) => CI ByteString -> a -> a
deleteHeader k = updateHeaders $ List.filter ((/= k) . fst)

-- | Adds a header key-value-pair to the 'HasHeaders' datatype. If a header
-- with the same name already exists, the new value is appended to the headers
-- list.
addHeader :: (HasHeaders a) => CI ByteString -> ByteString -> a -> a
addHeader k v = updateHeaders ((k,v):)

rqPathInfo :: Request -> ByteString
rqPathInfo = Char8.drop 1 . toByteString . encodePathSegments . pathInfo

setRqPathInfo :: ByteString -> Request -> Request
setRqPathInfo b r = r { pathInfo = decodePathSegments b }

-- | Modifes the 'Response' object stored in a 'Snap' monad.
modifyResponse :: MonadSnap m => (Response -> Response) -> m ()
modifyResponse f = liftSnap $
   smodify $ \ss -> ss { _snapResponse = f $ _snapResponse ss }
                    
-- | Modifies the 'Request' object stored in a 'Snap' monad.                    
modifyRequest :: MonadSnap m => (Request -> Request) -> m ()
modifyRequest f = liftSnap $
   smodify $ \ss -> ss { _snapRequest = f $ _snapRequest ss }
                    
-- | See 'rqParam'. Looks up a value for the given named parameter in the                    
-- 'Request'. If more than one value was entered for the given parameter name,
-- 'getParam' gloms the values together with:
--
-- @    'S.intercalate' \" \"@
--
getParam :: MonadSnap m
         => ByteString          -- ^ parameter name to look up
         -> m (Maybe ByteString)
getParam = getParamFrom rqParam
                    
getParamFrom :: MonadSnap m =>           
                (ByteString -> Request -> Maybe ByteString)
             -> ByteString
             -> m (Maybe ByteString)
getParamFrom f k = do
    rq <- getRequest
    return $! f k rq
           
type Params = Query      

rqParams :: Request -> Params
rqParams = queryString

setRqParams :: Params -> Request -> Request
setRqParams p req = req { queryString = p }

-- | Looks up the value(s) for the given named parameter. Parameters initially
    -- come from the request's query string and any decoded POST body (if the
    -- request's @Content-Type@ is @application\/x-www-form-urlencoded@).
    -- Parameter values can be modified within handlers using "rqModifyParams".
rqParam :: ByteString           -- ^ parameter name to look up
        -> Request              -- ^ HTTP request
        -> Maybe ByteString
rqParam k rq = join $ List.lookup k $ rqParams rq

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

-- | Expire the given 'Cookie' in client's browser.
expireCookie :: (MonadSnap m)
             => ByteString
             -- ^ Cookie name
             -> Maybe ByteString
             -- ^ Cookie domain
             -> m ()
expireCookie nm dm = do
  let old = UTCTime (ModifiedJulianDay 0) 0
  modifyResponse $ addResponseCookie
                 $ def { setCookieName = nm, setCookieDomain = dm, setCookieExpires = Just old }

-- | Adds the given lazy 'L.ByteString' to the body of the 'Response' stored
-- in the 'Snap' monad state.
--
-- Warning: This function is intentionally non-strict. If any pure
-- exceptions are raised by the expression creating the 'ByteString',
-- the exception won't actually be raised within the Snap handler.
writeLBS :: MonadSnap m => L.ByteString -> m ()
writeLBS s = writeBuilder $ fromLazyByteString s

writeBS :: MonadSnap m => ByteString -> m ()
writeBS s = writeBuilder $ fromByteString s

-- | Adds the given lazy 'LT.Text' to the body of the 'Response' stored in the
-- 'Snap' monad state.
--
-- Warning: This function is intentionally non-strict. If any pure
-- exceptions are raised by the expression creating the 'ByteString',
-- the exception won't actually be raised within the Snap handler.
writeLazyText :: MonadSnap m => LT.Text -> m ()
writeLazyText s = writeBuilder $ fromLazyText s

-- | Adds the given 'Builder' to the body of the 'Response' stored in the
-- | 'Snap' monad state.
writeBuilder :: MonadSnap m => Builder -> m ()
writeBuilder b = modifyResponse $ modifyResponseBody (`mappend` b)

statusReasonMap :: IM.IntMap ByteString
statusReasonMap = IM.fromList [
  (100, "Continue"),
  (101, "Switching Protocols"),
  (200, "OK"),
  (201, "Created"),
  (202, "Accepted"),
  (203, "Non-Authoritative Information"),
  (204, "No Content"),
  (205, "Reset Content"),
  (206, "Partial Content"),
  (300, "Multiple Choices"),
  (301, "Moved Permanently"),
  (302, "Found"),
  (303, "See Other"),
  (304, "Not Modified"),
  (305, "Use Proxy"),
  (307, "Temporary Redirect"),
  (400, "Bad Request"),
  (401, "Unauthorized"),
  (402, "Payment Required"),
  (403, "Forbidden"),
  (404, "Not Found"),
  (405, "Method Not Allowed"),
  (406, "Not Acceptable"),
  (407, "Proxy Authentication Required"),
  (408, "Request Time-out"),
  (409, "Conflict"),
  (410, "Gone"),
  (411, "Length Required"),
  (412, "Precondition Failed"),
  (413, "Request Entity Too Large"),
  (414, "Request-URI Too Large"),
  (415, "Unsupported Media Type"),
  (416, "Requested range not satisfiable"),
  (417, "Expectation Failed"),
  (500, "Internal Server Error"),
  (501, "Not Implemented"),
  (502, "Bad Gateway"),
  (503, "Service Unavailable"),
  (504, "Gateway Time-out"),
  (505, "HTTP Version not supported")
  ]
                  
-- | Sets the output to be the contents of the specified file, within the                  
-- given (start,end) range.
--
-- Calling 'sendFilePartial' will overwrite any output queued to be sent in
-- the 'Response'. If the response body is not modified after the call to
-- 'sendFilePartial', Snap will use the efficient @sendfile()@ system call on
-- platforms that support it.
--
-- If the response body is modified (using 'modifyResponseBody'), the file
-- will be read using @mmap()@.
sendFilePartial :: (MonadSnap m) => FilePath -> (Maybe FilePart) -> m ()
sendFilePartial f rng = modifyResponse go 
  where go (ResponseFile s h _ _) = ResponseFile s h f rng
        go (ResponseBuilder s h _) = ResponseFile s h f rng
        go (ResponseStream s h _) = ResponseFile s h f rng
        go _ = ResponseFile (mkStatus 200 "OK") mempty f rng
                        
-- If the response body is modified (using 'modifyResponseBody'), the file
-- will be read using @mmap()@.
sendFile :: (MonadSnap m) => FilePath -> m ()
sendFile f = sendFilePartial f Nothing

-- | Runs a 'Snap' monad action only when the 'rqPathInfo' of the request
-- starts with the given path. For example,
--
-- > dir "foo" handler
--
-- Will fail if 'rqPathInfo' is not \"@\/foo@\" or \"@\/foo\/...@\", and will
-- add @\"foo\/\"@ to the handler's local 'rqContextPath'.
dir :: MonadSnap m
    => ByteString  -- ^ path component to match
    -> m a         -- ^ handler to run
    -> m a
dir = pathWith f
  where
    f dr pinfo = dr == x
      where
        (x,_) = Char8.break (=='/') pinfo

-- | Runs a 'Snap' monad action only if the request's HTTP method matches
-- the given method.
method :: MonadSnap m => Method -> m a -> m a
method m action = do
      req <- getRequest
      unless (rqMethod req == m) pass
      action
      
rqMethod :: Request -> Method
rqMethod = parseMethod . requestMethod      

rqContextPath :: Request -> ByteString
rqContextPath r = 
  toByteString $ encodePathSegments $ reverse $ drop (length $ pathInfo r) $ reverse $ decodePathSegments $ rawPathInfo r
  
-- | Performs a redirect by setting the @Location@ header to the given target  
-- URL/path and the status code to 302 in the 'Response' object stored in a
-- 'Snap' monad. Note that the target URL is not validated in any way.
-- Consider using 'redirect\'' instead, which allows you to choose the correct
-- status code.
redirect :: MonadSnap m => ByteString -> m a
redirect target = redirect' target 302

-- | Performs a redirect by setting the @Location@ header to the given target
-- URL/path and the status code (should be one of 301, 302, 303 or 307) in the
-- 'Response' object stored in a 'Snap' monad. Note that the target URL is not
-- validated in any way.
redirect' :: MonadSnap m => ByteString -> Int -> m a
redirect' target status = do
      r <- getResponse
      
      finishWith
        $ setResponseCode status
        $ setContentLength 0
        $ modifyResponseBody (const mempty)
        $ setHeader "Location" target r
        
-- | Runs a 'Snap' monad action only for requests where 'rqPathInfo' is        
-- exactly equal to the given string. If the path matches, locally sets
-- 'rqContextPath' to the old value of 'rqPathInfo', sets 'rqPathInfo'=\"\",
-- and runs the given handler.
path :: MonadSnap m
     => ByteString  -- ^ path to match against
     -> m a         -- ^ handler to run
     -> m a
path = pathWith (==)

-- Runs a 'Snap' monad action only if the 'rqPathInfo' matches the given
-- predicate.
pathWith :: MonadSnap m
         => (ByteString -> ByteString -> Bool)
         -> ByteString
         -> m a
         -> m a
pathWith c p action = do
  req <- getRequest
  unless (c p (rqPathInfo req)) pass
  localRequest (updateContextPath $ Char8.length p) action
  
-- | Runs a 'Snap' action with a locally-modified 'Request' state  
-- object. The 'Request' object in the Snap monad state after the call
-- to localRequest will be unchanged.
localRequest :: MonadSnap m => (Request -> Request) -> m a -> m a
localRequest f m = do
    req <- getRequest
    runAct req <|> (putRequest req >> pass)
  where
    runAct req = do
      modifyRequest f
      result <- m
      putRequest req
      return result
      
-- Appends n bytes of the path info to the context path with a      
-- trailing slash.
updateContextPath :: Int -> Request -> Request
updateContextPath n req | n > 0     = setRqPathInfo pinfo req
                        | otherwise = req
  where
    pinfo = Char8.drop (n+1) (rqPathInfo req)
    
-- | route    
route :: MonadSnap m => [(ByteString, m a)] -> m a    
route rts = do
    p <- getsRequest rqPathInfo
    route' (return ()) [] (splitPath p) [] rts'
  where
    rts' = mconcat (map pRoute rts)
    
------------------------------------------------------------------------------
route' :: MonadSnap m
       => m ()           -- ^ action to run before we call the user handler
       -> [ByteString]   -- ^ the \"context\"; the list of path segments we've
                         -- already successfully matched, in reverse order
       -> [ByteString]   -- ^ the list of path segments we haven't yet matched
       -> Params
       -> Route a m
       -> m a
route' pre !ctx _ !params (Action action) =
    localRequest (updateContextPath (B.length ctx') . updateParams)
                   (pre >> action)
  where
    ctx' = B.intercalate (B.pack [c2w '/']) (reverse ctx)
    updateParams req = setRqParams (List.unionBy ((==) `on` fst) params (rqParams req)) req

route' pre !ctx [] !params (Capture _ _  fb) =
  route' pre ctx [] params fb

route' pre !ctx paths@(cwd:rest) !params (Capture p rt fb)
    | B.null cwd = fallback
    | otherwise  = m <|> fallback
  where
    fallback = route' pre ctx paths params fb
    m = maybe pass
              (\cwd' -> let params' = (p,Just cwd):params
                        in route' pre (cwd:ctx) rest params' rt)
              (urlDecode cwd)

route' pre !ctx [] !params (Dir _ fb) =
  route' pre ctx [] params fb
route' pre !ctx (cwd:rest) !params (Dir rtm fb) = do
    cwd' <- maybe ((liftIO $ putStrLn "pass!") >> pass) return $ urlDecode cwd
    case H.lookup cwd' rtm of
      Just rt -> (route' pre (cwd:ctx) rest params rt) <|>
                 (route' pre ctx (cwd:rest) params fb)
      Nothing -> route' pre ctx (cwd:rest) params fb

route' _ _ _ _ NoRoute = 
    pass
    

data Route a m = Action ((MonadSnap m) => m a)   -- wraps a 'Snap' action
               -- captures the dir in a param
               | Capture ByteString (Route a m) (Route a m)
               -- match on a dir
               | Dir (HashMap ByteString (Route a m)) (Route a m)
               | NoRoute



------------------------------------------------------------------------------
splitPath :: ByteString -> [ByteString]
splitPath = B.splitWith (== (c2w '/'))

------------------------------------------------------------------------------
pRoute :: MonadSnap m => (ByteString, m a) -> Route a m
pRoute (r, a) = foldr f (Action a) hier
  where
    hier   = filter (not . B.null) $ B.splitWith (== (c2w '/')) r
    f s rt = if B.head s == c2w ':'
        then Capture (B.tail s) rt NoRoute
        else Dir (H.fromList [(s, rt)]) NoRoute

------------------------------------------------------------------------------
instance Monoid (Route a m) where
    mempty = NoRoute

    mappend NoRoute r = r

    mappend l@(Action a) r = case r of
      (Action a')       -> Action (a <|> a')
      (Capture p r' fb) -> Capture p r' (mappend fb l)
      (Dir _ _)         -> mappend (Dir H.empty l) r
      NoRoute           -> l

    -- Whenever we're unioning two Captures and their capture variables
    -- differ, we have an ambiguity. We resolve this in the following order:
    --   1. Prefer whichever route is longer
    --   2. Else, prefer whichever has the earliest non-capture
    --   3. Else, prefer the right-hand side
    mappend l@(Capture p r' fb) r = case r of
      (Action _)           -> Capture p r' (mappend fb r)
      (Capture p' r'' fb')
              | p == p'    -> Capture p (mappend r' r'') (mappend fb fb')
              | rh' > rh'' -> Capture p r' (mappend fb r)
              | rh' < rh'' -> Capture p' r'' (mappend fb' l)
              | en' < en'' -> Capture p r' (mappend fb r)
              | otherwise  -> Capture p' r'' (mappend fb' l)
        where
          rh'  = routeHeight r'
          rh'' = routeHeight r''
          en'  = routeEarliestNC r' 1
          en'' = routeEarliestNC r'' 1
      (Dir rm fb')         -> Dir rm (mappend fb' l)
      NoRoute              -> l

    mappend l@(Dir rm fb) r = case r of
      (Action _)      -> Dir rm (mappend fb r)
      (Capture _ _ _) -> Dir rm (mappend fb r)
      (Dir rm' fb')   -> Dir (H.unionWith mappend rm rm') (mappend fb fb')
      NoRoute         -> l

------------------------------------------------------------------------------
routeHeight :: Route a m -> Int
routeHeight r = case r of
  NoRoute          -> 1
  (Action _)       -> 1
  (Capture _ r' _) -> 1 + routeHeight r'
  (Dir rm _)       -> 1 + foldl max 1 (map routeHeight $ H.elems rm)


------------------------------------------------------------------------------
routeEarliestNC :: Route a m -> Int -> Int
routeEarliestNC r n = case r of
  NoRoute           -> n
  (Action _)        -> n
  (Capture _ r' _)  -> routeEarliestNC r' n+1
  (Dir _ _)         -> n
