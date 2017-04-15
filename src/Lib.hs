{-# LANGUAGE OverloadedStrings #-}

module Lib ( scottyMain ) where

import qualified Web.Scotty.Trans as S
import Web.Scotty.Internal.Types (ScottyT)
import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)
import Data.Text.Internal.Lazy (Text)
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

-- https://hackage.haskell.org/package/scotty

type BallPosition = (Double, Double)

-- type BallPit = StateT BallPosition S.ActionM

-- http://stackoverflow.com/questions/27495381/use-statet-within-web-scotty
-- https://codereview.stackexchange.com/questions/82357/small-web-service-using-scotty?newreg=94959da35e984e06887b636d4cc71bea

scottyMain :: IO ()
scottyMain = undefined

  -- do
  -- let s0 = "message"
  --     transform = flip evalStateT s0
  -- runner <- restartableStateT s0
  -- S.scottyT 3000 transform runner routes


restartableStateT :: s -> IO (StateT s IO a -> IO a)
restartableStateT s0 = do
  r <- newIORef s0
  return $ \act -> do
    s <- readIORef r
    (x, s') <- runStateT act s
    atomicModifyIORef' r $ const (s', x)
    



-- routes :: ScottyT Text IO ()
-- routes = do
--   S.get "/beam/:word" $ do
--     beam <- S.param "word"
--     S.html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
--   S.get "/" $ S.file "static/ballPit.html"
--   S.get "/foo" $ do
--     S.html $ "hello from Scotty"
--   S.post "/moveBall" $ do
--     -- http://stackoverflow.com/questions/39378493/ambiguous-type-variable-a0-arising-from-a-use-of-param-prevents-the-constrai
--     x <- S.param "x" :: S.ActionT Double
--     y <- S.param "y" :: S.ActionT Double
--     _ <- liftIO $ putStrLn $ show x ++ " " ++ show y
--     S.html $ pack $ (show x) ++ " " ++ (show y)
--   -- S.get "/ballX" $ lift $ do
--   --   (x, y) <- get
--   --   return (S.text 1.234)
--   S.get "/ballY" $ S.text "300"
