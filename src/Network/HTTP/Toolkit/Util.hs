module Network.HTTP.Toolkit.Util where

while :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
while p src action = go
  where
    go = do
      x <- src
      if p x
        then action x >> go
        else return ()
