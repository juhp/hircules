module MaybeDo (maybeDo) where

maybeDo :: Monad m => (a -> m ()) -> Maybe a -> m ()
maybeDo act mb = maybe (return ()) act mb
