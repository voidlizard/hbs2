module Dialog.Helpers.List where

import Control.Monad.Trans.Writer.CPS qualified as W
import Data.Functor.Identity
import Data.Monoid

type ListBuilder a = ListBuilderT Identity a

type ListBuilderT m a = W.WriterT (Endo [a]) m ()

buildList :: ListBuilder a -> [a]
buildList = runIdentity . buildListT

buildListT :: Monad m => ListBuilderT m a -> m [a]
buildListT = fmap (flip appEndo []) . W.execWriterT

li :: Monad m => a -> ListBuilderT m a
li = W.tell . Endo . (:)

