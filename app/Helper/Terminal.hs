module Helper.Terminal where

import System.Console.ANSI
import System.Console.Terminal.Size

separator :: IO ()
separator = do
    windowSize <- size
    let terminalWidth = maybe 0 width windowSize
    putStrLn $ replicate terminalWidth '\x2500'

header :: String -> IO ()
header str = do
    setSGR [SetColor Foreground Dull Green]
    putStrLn str
    setSGR []
