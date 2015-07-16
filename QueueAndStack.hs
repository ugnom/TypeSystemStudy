module QueueAndStack where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

push :: a -> StateT [a] Maybe ()
push x = do
	xs <- get
	put (x : xs)
	lift (Just ())


pop :: Eq a => StateT [a] Maybe a
pop = do
	xs <- get
	if xs == [] then do
		lift Nothing
	else do
		put . tail $ xs
		lift . return . head $ xs
