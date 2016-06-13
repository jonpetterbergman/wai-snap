{-# language OverloadedStrings #-}
module Snap.Core where

import           Blaze.ByteString.Builder     (Builder)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8      as Char8

import           Control.Monad.State          (StateT,runStateT)
import           Data.CaseInsensitive         (CI)  
import           Data.HashMap.Strict          (HashMap)   
import qualified Data.HashMap.Strict        as HashMap
import           Data.Map                     (Map)
import qualified Data.Map                   as Map
import           Data.Time                    (UTCTime)
import           Data.Int                     (Int64)

import           Network.Wai                  (Application)
import qualified Network.Wai               as  Wai

newtype Snap a = Snap {
  unSnap :: StateT SnapState IO (SnapResult a)
  }
                 
data ResponseBody =                 
    ResponseEmpty
  | ResponseBuilder Builder                
  | ResponseFile FilePath
    
data SnapState = SnapState                 
  { _snapRequest :: Request
  , _snapResponse :: Response
  , _snapLogError :: ByteString -> IO ()
  , _snapModifyTimeout :: (Int -> Int) -> IO ()
  }
  
snapToWai :: Snap a -> Application  
snapToWai (Snap m) aReq aCont = do
    (r,ss') <- runStateT m $ SnapState req dresp dummyLog (const $ return ())
    let resp = case r of
                 SnapValue _ -> _snapResponse ss'
                 PassOnProcessing _ -> fourohfour
                 EarlyTermination x -> x
    let req' = _snapRequest ss'
    aCont $ convertResponse resp
  where dresp = emptyResponse { rspHttpVersion = rqVersion req }
        req = convertRequest aReq
        
fourohfour :: Response        
fourohfour = setResponseBody body404 $ emptyResponse        
        
body404 :: Builder             
body404 = undefined
             
-- | Sets an HTTP response body to the given 'Builder' value.             
setResponseBody     :: Builder  -- ^ new response body builder
                    -> Response -- ^ response to modify
                    -> Response
setResponseBody b r = r { rspBody = ResponseBuilder b }

dummyLog :: ByteString -> IO ()        
dummyLog = Char8.putStrLn
        
convertRequest :: Wai.Request -> Request
convertRequest = undefined                            

convertResponse :: Response -> Wai.Response
convertResponse = undefined  

-- | An empty 'Response'.
emptyResponse :: Response
emptyResponse = Response emptyHeaders Map.empty (1,1) Nothing
                         ResponseEmpty
                         200 "OK" False True
                            
-- | Enumerates the HTTP method values (see
-- <http://tools.ietf.org/html/rfc2068.html#section-5.1.1>).
data Method  = GET | HEAD | POST | PUT | DELETE | TRACE | OPTIONS | CONNECT |
               PATCH | Method ByteString
               deriving(Show,Read,Ord)
                                              
                                              
instance Eq Method where
    GET          == GET              = True
    GET          == Method "GET"     = True
    HEAD         == HEAD             = True
    HEAD         == Method "HEAD"    = True
    POST         == POST             = True
    POST         == Method "POST"    = True
    PUT          == PUT              = True
    PUT          == Method "PUT"     = True
    DELETE       == DELETE           = True
    DELETE       == Method "DELETE"  = True
    TRACE        == TRACE            = True
    TRACE        == Method "TRACE"   = True
    OPTIONS      == OPTIONS          = True
    OPTIONS      == Method "OPTIONS" = True
    CONNECT      == CONNECT          = True
    CONNECT      == Method "CONNECT" = True
    PATCH        == PATCH            = True
    PATCH        == Method "PATCH"   = True
    Method a     == Method b         = a == b
    m@(Method _) == other            = other == m
    _            == _                = False

data SnapResult a = SnapValue a  
                  | PassOnProcessing String
                  | EarlyTermination Response
                    
newtype Headers = H { unH :: HashMap (CI ByteString) [ByteString] }                    
  deriving (Show)
                    
emptyHeaders = H (HashMap.empty)           
           
-- | Contains all of the information about an incoming HTTP request.
data Request = Request
    { -- | The server name of the request, as it came in from the request's
      -- @Host:@ header.
      rqServerName     :: ByteString
      
      -- | Returns the port number the HTTP server is listening on.
    , rqServerPort     :: !Int
      
      -- | The remote IP address.
    , rqRemoteAddr     :: ByteString
      
      -- | The remote TCP port number.
    , rqRemotePort     :: Int
      
      -- | The local IP address for this request.
    , rqLocalAddr      :: ByteString
      
      -- | Returns the port number the HTTP server is listening on.
    , rqLocalPort      :: Int
      
      -- | Returns the HTTP server's idea of its local hostname.
    , rqLocalHostname  :: ByteString
      
      -- | Returns @True@ if this is an @HTTPS@ session.
    , rqIsSecure       :: Bool
    , rqHeaders        :: Headers
    , rqBody           :: (IO ByteString)
                  
      -- | Returns the @Content-Length@ of the HTTP request body.
    , rqContentLength  :: !(Maybe Int)
      
      -- | Returns the HTTP request method.
    , rqMethod         :: !Method
      
      -- | Returns the HTTP version used by the client.
    , rqVersion        :: HttpVersion
      
      -- | Returns a list of the cookies that came in from the HTTP request
      -- headers.
    , rqCookies        :: [Cookie]
      
      -- | Handlers can be hung on a @URI@ \"entry point\"; this is called the
      -- \"context path\". If a handler is hung on the context path
      -- @\"\/foo\/\"@, and you request @\"\/foo\/bar\"@, the value of
      -- 'rqPathInfo' will be @\"bar\"@.
      --
      -- The following identity holds:
      --
      -- > rqURI r == S.concat [ rqContextPath r
      -- >                     , rqPathInfo r
      -- >                     , let q = rqQueryString r
      -- >                       in if S.null q
      -- >                            then ""
      -- >                            else S.append "?" q
      -- >                     ]
    , rqPathInfo       :: !ByteString
      
      -- | The \"context path\" of the request; catenating 'rqContextPath',
      -- and 'rqPathInfo' should get you back to the original 'rqURI'
      -- (ignoring query strings). The 'rqContextPath' always begins and ends
      -- with a slash (@\"\/\"@) character, and represents the path (relative
      -- to your component\/snaplet) you took to get to your handler.
    , rqContextPath    :: !ByteString
      
      -- | Returns the @URI@ requested by the client.
    , rqURI            :: !ByteString
      
      -- | Returns the HTTP query string for this 'Request'.
    , rqQueryString    :: !ByteString
      
      -- | Returns the parameters mapping for this 'Request'. \"Parameters\"
      -- are automatically decoded from the URI's query string and @POST@ body
      -- and entered into this mapping. The 'rqParams' value is thus a union of
      -- 'rqQueryParams' and 'rqPostParams'.
    , rqParams         :: Params
      
      -- | The parameter mapping decoded from the URI's query string.
    , rqQueryParams    :: Params
      
      -- | The parameter mapping decoded from the POST body. Note that Snap
      -- only auto-decodes POST request bodies when the request's
      -- @Content-Type@ is @application/x-www-form-urlencoded@.
      -- For @multipart/form-data@ use 'Snap.Util.FileUploads.handleFileUploads'
      -- to decode the POST request and fill this mapping.
    , rqPostParams     :: Params
    }
    
-- | A datatype representing an HTTP cookie.    
data Cookie = Cookie {
      -- | The name of the cookie.
      cookieName      :: !ByteString
      
      -- | The cookie's string value.
    , cookieValue     :: !ByteString
      
      -- | The cookie's expiration value, if it has one.
    , cookieExpires   :: !(Maybe UTCTime)
      
      -- | The cookie's \"domain\" value, if it has one.
    , cookieDomain    :: !(Maybe ByteString)
      
      -- | The cookie path.
    , cookiePath      :: !(Maybe ByteString)
      
      -- | Tag as secure cookie?
    , cookieSecure    :: !Bool
      
      -- | HttpOnly?
    , cookieHttpOnly  :: !Bool
    } deriving (Eq, Show)
               
type HttpVersion = (Int,Int)               

-- | A type alias for the HTTP parameters mapping. Each parameter
-- key maps to a list of ByteString values; if a parameter is specified
-- multiple times (e.g.: \"@GET /foo?param=bar1&param=bar2@\"), looking up
-- \"@param@\" in the mapping will give you @[\"bar1\", \"bar2\"]@.
type Params = Map ByteString [ByteString]

-- | Represents an HTTP response.
data Response = Response
    { rspHeaders            :: Headers
    , rspCookies            :: Map ByteString Cookie
    , rspHttpVersion        :: !HttpVersion
                  
      -- | We will need to inspect the content length no matter what, and
      --   looking up \"content-length\" in the headers and parsing the number
      --   out of the text will be too expensive.
    , rspContentLength      :: !(Maybe Int64)
    , rspBody               :: ResponseBody
            
      -- | Returns the HTTP status code.
    , rspStatus             :: !Int
      
      -- | Returns the HTTP status explanation string.
    , rspStatusReason       :: !ByteString
      
      -- | If true, we are transforming the request body with
      -- 'transformRequestBody'
    , rspTransformingRqBody :: !Bool
      
      -- | Controls whether Snap will buffer the output or not. You may wish to
      -- disable buffering when using Comet-like techniques which rely on the
      -- immediate sending of output data in order to maintain interactive
      -- semantics.
    , rspOutputBuffering    :: !Bool
    }
    
    