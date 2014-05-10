{-# LANGUAGE OverloadedStrings #-}
module AppArgs (getAppArgs, GetOptOptions(..)) where

import System.IO
import System.Console.GetOpt
import System.Exit
import System.Environment
import Data.Word
import Control.Monad

data GetOptOptions = GetOptOptions  { optPort           :: Int
                                    , optDBHost         :: String
                                    , optDBPort         :: Word16
                                    , optDBUser         :: String
                                    , optDBPassword     :: String
                                    } deriving (Show)
startOptions :: GetOptOptions
startOptions = GetOptOptions  { optPort         = 8080
                              , optDBHost       = "localhost"
                              , optDBPort       = 5432
                              , optDBUser       = "psql"
                              , optDBPassword   = "" }

options :: [ OptDescr (GetOptOptions -> IO GetOptOptions) ]
options =
    [ Option "p" ["port"]
        (ReqArg
            (\arg opt -> case reads arg :: [(Int, String)] of
                           [(n, "")] -> return opt { optPort = n }
                           _ -> do
                             hPutStrLn stderr "Failed to parse port"
                             exitWith ExitSuccess
            )
            "PORT")
        "Server port"

    , Option "d" ["dbhost"]
        (ReqArg
            (\arg opt -> return opt { optDBHost = arg })
            "HOST")
        "Database server host"

    , Option "f" ["dbport"]
        (ReqArg
            (\arg opt -> case reads arg :: [(Word16, String)] of
                            [(n, "")] -> return opt { optDBPort = n }
                            _ -> do
                              hPutStrLn stderr "Failed to parse dbPort"
                              exitWith ExitSuccess
            )
            "PORT")
        "Database server port"

    , Option "u" ["dbuser"]
        (ReqArg
            (\arg opt -> return opt { optDBUser = arg })
            "USERNAME")
        "Database user name"

    , Option "" ["dbpasswd"]
        (ReqArg
            (\arg opt -> return opt { optDBPassword = arg })
            "PASSWORD")
        "Database password"

    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.01"
                exitWith ExitSuccess))
        "Print version"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

getAppArgs :: [String] -> IO GetOptOptions
getAppArgs args = do
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args


    let(haveUnresolvedOptions) = length nonOptions > 0

    when haveUnresolvedOptions ( do
                            hPutStrLn stderr $ "Unresolved options: " ++ concat nonOptions
                            hPutStrLn stderr $ usageInfo "" options
                            exitWith ExitSuccess)

    -- Here we thread startOptions through all supplied option actions
    foldl (>>=) (return startOptions) actions
    
