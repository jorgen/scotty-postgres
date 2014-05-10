{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Database.PostgreSQL.Simple

import AppArgs
import WebServerApi

main :: IO ()
main = do
    args <- getArgs

    appArgs <- getAppArgs args

    conn <- connect defaultConnectInfo
        { connectHost = (optDBHost appArgs),
          connectPort = optDBPort appArgs,
          connectUser = optDBUser appArgs 
        }


    scotty (optPort appArgs) $ do
        middleware logStdoutDev
        get "/:word" $ getSumAction conn

