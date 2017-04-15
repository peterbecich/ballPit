{-# LANGUAGE OverloadedStrings #-}

module Lib ( scottyMain ) where

import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- https://hackage.haskell.org/package/scotty
scottyMain :: IO ()
scottyMain = scotty 3000 $ do
  -- http://stackoverflow.com/questions/22662826/web-scotty-file-not-found-while-serving-static-files
  get "/" $ file "static/ballPit.html"
  get "/foo" $ do
    html $ "hello from Scotty"
  -- get "/:word" $ do
  --   beam <- param "word"
  --   html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
  post "/moveBall" $ do
    -- http://stackoverflow.com/questions/39378493/ambiguous-type-variable-a0-arising-from-a-use-of-param-prevents-the-constrai
    x <- param "x" :: ActionM Double
    y <- param "y" :: ActionM Double
    _ <- liftIO $ putStrLn $ show x ++ " " ++ show y
    html $ pack $ (show x) ++ " " ++ (show y)
