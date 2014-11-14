module Dodgy.Utils (mapR, mapMR) where

mapR :: [a] -> (a -> b) -> [b]
mapR l f = map f l

mapMR :: Monad m => [a] -> (a -> m b) -> m [b]
mapMR l f = mapM f l