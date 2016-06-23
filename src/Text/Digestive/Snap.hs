module Text.Digestive.Snap where

import Snap.Core (MonadSnap,
                  getRequest)
import Data.Text (Text)
import Text.Digestive.Form(Form)
import Text.Digestive.View(View,postForm)
import Network.Wai.Digestive(requestFormEnv_)
import Control.Monad.Trans.Resource(ResourceT)
import Control.Monad.Trans(lift)

runForm :: MonadSnap m 
        => Text
        -> Form v (ResourceT m) a
        -> ResourceT m (View v,Maybe a)
runForm name frm = 
  do
    r <- lift $ getRequest
    postForm name frm (const $ requestFormEnv_ r) 
