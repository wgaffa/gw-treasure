{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import GHC.IO.Encoding

import Data.Maybe

import Command

dispatch :: [(String, [String] -> IO ())]
dispatch = [
    ("view", view)
    ]

main :: IO ()
main = do
    setLocaleEncoding utf8
    commandline <- getArgs
    let (action, args) = fromMaybe (usage, [""]) $ arguments dispatch commandline
    action args
