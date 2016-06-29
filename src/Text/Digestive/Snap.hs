{-# language RankNTypes #-}
{-# language GADTs #-}
{-# language TupleSections #-}
{-# language OverloadedStrings #-}
module Text.Digestive.Snap where

import Snap.Core (MonadSnap,
                  getRequest,
                  rqMethod)
import Data.Text (Text)
import Text.Digestive.Form(Form)
import Text.Digestive.Form.Internal(FormTree(..))
import Text.Digestive.Form.List(DefaultList)
import Text.Digestive.View(View,postForm,getForm)
import Network.Wai(queryString)
import Network.Wai.Digestive(bodyFormEnv_,queryFormEnv)
import Control.Monad.Trans.Resource(ResourceT)
import Control.Monad.Trans(lift,liftIO)
import Network.HTTP.Types.Method(StdMethod(..))
import Network.HTTP.Types.QueryLike(QueryLike())

runForm :: MonadSnap m 
        => Text
        -> Form v m a
        -> ResourceT m (View v,Maybe a)
runForm name frm = 
  do
    r <- lift $ getRequest
    case rqMethod r of
      (Right POST) -> postForm name (mapFM lift frm) (const $ bodyFormEnv_ r) 
      _ -> fmap (,Nothing) $ getForm name (mapFM lift frm)

runFormGet :: MonadSnap m 
           => Text
           -> Form v m a
           -> ResourceT m (View v,Maybe a)
runFormGet name frm = 
  do
    r <- lift $ getRequest
    let env = queryFormEnv $ queryString r
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
