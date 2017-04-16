{-# LANGUAGE OverloadedStrings #-}

module Example where

import Web.Scotty.Trans
import Control.Monad.State (StateT, evalStateT, runStateT, lift)
import qualified Control.Monad.State as S
import Data.Text.Lazy (Text)
import Data.IORef

-- http://stackoverflow.com/questions/27495381/use-statet-within-web-scotty

main :: IO ()
main = do
    let s0 = "message"
    let transform = flip evalStateT s0
    runner <- restartableStateT s0
    scottyT 3000 transform runner routes

restartableStateT :: s -> IO (StateT s IO a -> IO a)
restartableStateT s0 = do
    r <- newIORef s0
    return $ \act -> do
        s <- readIORef r
        (x, s') <- runStateT act s
        atomicModifyIORef' r $ const (s', x)


routes :: ScottyT Text (StateT Text IO) ()
routes = do

  get "/data" $ do
    val <- lift S.get
    text val

  put "/data/:val" $ do
    val <- param "val"
    lift $ S.put val
    text val
