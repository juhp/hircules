module MaybeDo (maybeDo, maybeDo_) where

maybeDo :: Monad m => b -> Maybe a -> (a -> m b) -> m b
maybeDo b mb act = maybe (return b) act mb

maybeDo_ :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeDo_ mb act = maybeDo () mb act
