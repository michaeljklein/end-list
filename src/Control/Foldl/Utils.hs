module Control.Foldl.Utils (feed, feedM, extractM) where

import Control.Foldl (Fold(..), FoldM(..), purely, impurely, foldM)
import Data.Proxy (Proxy(..))

-- | Feed an input value to a `Fold`, updating its internal state
feed :: Fold a b -> a -> Fold a b
feed f x = purely (\stp ini -> Fold stp (stp ini x)) f

-- | Feed an input value to a `FoldM`, updating its internal `Monad`ic state
feedM :: Monad m => FoldM m a b -> a -> FoldM m a b
feedM f x = impurely (\stp ini -> FoldM stp (ini >>= flip stp x)) f

-- | Extract the current result from a `FoldM`
extractM :: Monad m => FoldM m a b -> m b
extractM f = foldM f Proxy

