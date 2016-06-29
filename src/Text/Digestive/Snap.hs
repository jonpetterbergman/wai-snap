{-# language RankNTypes #-}
{-# language GADTs #-}
{-# language TupleSections #-}
{-# language OverloadedStrings #-}
module Text.Digestive.Snap where

import Snap.Core (MonadSnap,
                  getRequest,
                  rqMethod)
import Data.ByteString(ByteString)  
import Data.Text (Text)
import Data.List(isPrefixOf)
import Data.Maybe(mapMaybe)
import Data.Text.Encoding(decodeUtf8With)
import Data.Text.Encoding.Error(lenientDecode)
import Text.Digestive.Form(Form)
import Text.Digestive.Form.Internal(FormTree(..))
import Text.Digestive.Form.List(DefaultList)
import Text.Digestive.Types(Path,Env,FormInput(..),toPath)
import Text.Digestive.View(View,postForm,getForm)
import Network.Wai(queryString,Request)
import Network.Wai.Parse(parseRequestBody,tempFileBackEnd,File,fileContent)
import Control.Monad.Trans.Resource(ResourceT,getInternalState)
import Control.Monad.Trans(lift,liftIO,MonadIO)
import Network.HTTP.Types.Method(StdMethod(..))
import Network.HTTP.Types.QueryLike(QueryLike(..),toQueryValue)
import Network.HTTP.Types.URI(Query)
import Data.Function(on)

newtype FileQuery = FileQuery [File FilePath]

instance QueryLike FileQuery where
  toQuery (FileQuery files) =
    map (\(k,v) -> (k,toQueryValue $ fileContent v)) files


runForm :: MonadSnap m 
        => Text
        -> Form v m a
        -> ResourceT m (View v,Maybe a)
runForm name frm = 
  do
    r <- lift $ getRequest
    case rqMethod r of
      (Right POST) -> postForm name (mapFM lift frm) (const $ bodyFormEnv__ r) 
      _ -> fmap (,Nothing) $ getForm name (mapFM lift frm)

runFormGet :: MonadSnap m 
           => Text
           -> Form v m a
           -> ResourceT m (View v,Maybe a)
runFormGet name frm = 
  do
    r <- lift $ getRequest
    let env = queryFormEnv_ $ queryString r
    act <- env []
    liftIO $ print act
    case rqMethod r of
      (Right GET) -> postForm name (mapFM lift frm) (const $ return env)
      _ -> fmap (,Nothing) $ getForm name (mapFM lift frm)


mapFM :: Functor m => (forall a. m a -> m' a) -> Form v m b -> Form v m' b
mapFM f (Ref ref ft) = Ref ref $ mapFM f ft
mapFM f (Pure field) = Pure field
mapFM f (App t ft) = App (mapFM f t) (mapFM f ft)
mapFM f (Map t ft) = Map (\x -> f $ t x) (mapFM f ft)
mapFM f (Monadic act) = Monadic $ f (fmap (mapFM f) act)
mapFM f (List dl ft) = List (fmap (mapFM f) dl) (mapFM f ft)
mapFM f (Metadata md ft) = Metadata md $ mapFM f ft

bodyFormEnv__ :: (Monad m,MonadIO io) => Request -> ResourceT io (Env m) 
bodyFormEnv__ req = do
  st <- getInternalState
  (query, files) <- liftIO $ parseRequestBody (tempFileBackEnd st) req
  return $ queryFormEnv_ (toQuery query ++ toQuery (FileQuery files))

-- | Build an 'Text.Digestive.Types.Env' from a query
queryFormEnv_ :: (QueryLike q, Monad m) => q -> Env m
queryFormEnv_ qs = \pth ->
  return $ map (TextInput . decodeUtf8With lenientDecode) $ matchPath pth qs'
  where
    qs' = toQuery qs
    
matchPath :: Path -> Query -> [ByteString]    
matchPath pth = mapMaybe snd . filter go
  where go (key,val) = pth `isPrefixOf` (toPath $ decodeUtf8With lenientDecode key)