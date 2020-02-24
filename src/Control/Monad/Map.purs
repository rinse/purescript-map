module Control.Monad.Map
    ( Map
    , MapT
    , runMap
    , runMapT
    ) where

import Prelude

import Control.Alt                (class Alt)
import Control.Alternative        (class Alternative)
import Control.Plus               (class Plus)
import Control.Lazy               (class Lazy)
import Control.Monad.Cont.Class   (class MonadCont)
import Control.Monad.Error.Class  (class MonadError, class MonadThrow)
import Control.Monad.Map.Class    (class MonadMap)
import Control.Monad.Rec.Class    (class MonadRec)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter)
import Control.Monad.State        (StateT, get, modify_, runStateT)
import Control.Monad.State.Class  (class MonadState)
import Control.Monad.Trans.Class  (class MonadTrans)
import Control.MonadPlus          (class MonadPlus)
import Control.MonadZero          (class MonadZero)
import Data.Identity              (Identity)
import Data.Map                   as D
import Data.Newtype               (class Newtype, unwrap)
import Data.Tuple                 (Tuple)
import Effect.Class               (class MonadEffect)


newtype MapT k v m a = MapT (StateT (D.Map k v) m a)
type Map k v a = MapT k v Identity a

runMapT :: forall k v m a. MapT k v m a -> D.Map k v -> m (Tuple a (D.Map k v))
runMapT = runStateT <<< unwrap

runMap :: forall k v a. Map k v a -> D.Map k v -> Tuple a (D.Map k v)
runMap = map unwrap <<< runMapT

derive instance newtypeT :: Newtype (MapT k v m a) _
derive newtype instance functorMapT     :: Functor m => Functor (MapT k v m)
derive newtype instance applyMapT       :: Monad m => Apply (MapT k v m)
derive newtype instance applicativeMapT :: Monad m => Applicative (MapT k v m)
derive newtype instance bindMapT        :: Monad m => Bind (MapT k v m)
derive newtype instance monadMapT       :: Monad m => Monad (MapT k v m)
derive newtype instance altMapT         :: (Monad m, Alt m) => Alt (MapT k v m)
derive newtype instance plusMapT        :: (Monad m, Plus m) => Plus (MapT k v m)
derive newtype instance alternativeMapT :: (Monad m, Alternative m) => Alternative (MapT k v m)
derive newtype instance monadRecMapT    :: MonadRec m => MonadRec (MapT k v m)
derive newtype instance monadZeroMapT   :: MonadZero m => MonadZero (MapT k v m)
derive newtype instance monadPlusMapT   :: MonadPlus m => MonadPlus (MapT k v m)
derive newtype instance monadTransMapT  :: MonadTrans (MapT k v)
derive newtype instance lazyMapT        :: Lazy (MapT k v m a)
derive newtype instance monadEffectMapT :: MonadEffect m => MonadEffect (MapT k v m)
derive newtype instance monadContMapT   :: MonadCont m => MonadCont (MapT k v m)
derive newtype instance monadThrowMapT  :: MonadThrow e m => MonadThrow e (MapT k v m)
derive newtype instance monadErrorMapT  :: MonadError e m => MonadError e (MapT k v m)
derive newtype instance monadStateMapT  :: Monad m => MonadState (D.Map k v) (MapT k v m)
derive newtype instance monadAskMapT    :: MonadAsk r m => MonadAsk r (MapT k v m)
derive newtype instance monadReaderMapT :: MonadReader r m => MonadReader r (MapT k v m)
derive newtype instance monadTellMapT   :: MonadTell w m => MonadTell w (MapT k v m)
derive newtype instance monadWriterMapT :: MonadWriter w m => MonadWriter w (MapT k v m)

instance monadMapMapT :: (Ord k, Monad m) => MonadMap k v (MapT k v m) where
    delete = MapT <<< modify_ <<< D.delete
    insert k v = modify_ $ D.insert k v
    lookup k = D.lookup k <$> get
    values = D.values <$> get
