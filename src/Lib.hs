{-# LANGUAGE OverloadedStrings #-}

module Lib ( scottyMain ) where

--import qualified Web.Scotty.Trans as S
import qualified Web.Scotty as S
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
initialBallPosition = (100, 100)

-- type BallPit = StateT BallPosition S.ActionM

-- http://stackoverflow.com/questions/27495381/use-statet-within-web-scotty
-- https://codereview.stackexchange.com/questions/82357/small-web-service-using-scotty?newreg=94959da35e984e06887b636d4cc71bea

scottyMain :: IO ()
scottyMain = S.scotty 3000 routes

  -- do
  -- let s0 = "message"
  --     transform = flip evalStateT s0
  -- runner <- restartableStateT s0
  -- S.scottyT 3000 transform runner routes


-- restartableStateT :: s -> IO (StateT s IO a -> IO a)
-- restartableStateT s0 = do
--   r <- newIORef s0
--   return $ \act -> do
--     s <- readIORef r
--     (x, s') <- runStateT act s
--     atomicModifyIORef' r $ const (s', x)


routes :: ScottyT Text IO ()
routes = fmap (\_ -> ()) $ execStateT stateRoutes initialBallPosition

stateRoutes :: StateT BallPosition (ScottyT Text IO) ()
stateRoutes = do
  (x, y) <- get
  _ <- lift $ S.get "/ballX" $ S.text (pack (show x))
  lift $ S.get "/ballY" $ S.text (pack (show y))
  -- (x', y') <- lift $ S.post "/moveBall" $ do
  --   x'' <- S.param "x" :: S.ActionM Double
  --   y'' <- S.param "y" :: S.ActionM Double
  --   _ <- liftIO $ putStrLn $ show x ++ " " ++ show y
  --   S.html $ pack $ (show x) ++ " " ++ (show y)
  -- S.get "/" $ S.file "static/ballPit.html"

