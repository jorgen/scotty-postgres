{-# LANGUAGE OverloadedStrings #-}
module WebServerApi (getSumAction) where

import Web.Scotty

import Data.Monoid (mconcat)

import Control.Monad.Trans (liftIO)

import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as Text

import Database.PostgreSQL.Simple
import DatabaseApi

getSumAction :: Connection -> ActionM ()
getSumAction connection = do
    beam <- param "word"
    liftIO $ putStrLn "hello"
    number <- liftIO $ fmap (LazyText.pack . show) $ getSumFromDb connection
    html $ mconcat ["<h1>Scotty, ", beam, " And a number ", number, " me up!</h1>"]
