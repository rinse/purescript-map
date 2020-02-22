module Control.Monad.Map.Class
    ( class MonadMap
    , delete
    , insert
    , lookup
    , member
    , values
    ) where

import Prelude

import Data.List  (List)
import Data.Maybe (Maybe, isJust)


class MonadMap k v m | m -> k, m -> v where
    delete :: k -> m Unit
    insert :: k -> v -> m Unit
    lookup :: k -> m (Maybe v)
    values :: m (List v)

member :: forall k v m. MonadMap k v m => Functor m => k -> m Boolean
member = map isJust <<< lookup
