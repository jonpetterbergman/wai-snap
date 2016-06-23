{-# language RankNTypes #-}
{-# language GADTs #-}
module Text.Digestive.Snap where

import Snap.Core (MonadSnap,
                  getRequest)
import Data.Text (Text)
import Text.Digestive.Form(Form)
import Text.Digestive.Form.Internal(FormTree(..))
import Text.Digestive.Form.List(DefaultList)
import Text.Digestive.View(View,postForm)
import Network.Wai.Digestive(requestFormEnv_)
import Control.Monad.Trans.Resource(ResourceT)
import Control.Monad.Trans(lift)

runForm :: MonadSnap m 
        => Text
        -> Form v m a
        -> ResourceT m (View v,Maybe a)
runForm name frm = 
  do
    r <- lift $ getRequest
    postForm name (mapFM lift frm) (const $ requestFormEnv_ r) 

mapFM :: Functor m => (forall a. m a -> m' a) -> Form v m b -> Form v m' b
mapFM f (Ref ref ft) = Ref ref $ mapFM f ft
mapFM f (Pure field) = Pure field
mapFM f (App t ft) = App (mapFM f t) (mapFM f ft)
mapFM f (Map t ft) = Map (\x -> f $ t x) (mapFM f ft)
mapFM f (Monadic act) = Monadic $ f (fmap (mapFM f) act)
mapFM f (List dl ft) = List (fmap (mapFM f) dl) (mapFM f ft)
mapFM f (Metadata md ft) = Metadata md $ mapFM f ft
