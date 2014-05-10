{-# LANGUAGE OverloadedStrings #-}
module DatabaseApi (getSumFromDb) where

import Database.PostgreSQL.Simple

getSumFromDb :: Connection -> IO Int
getSumFromDb connection = do
    xs <- query_ connection "select 2 + 2"
    return $ (fromOnly $ head xs :: Int)
