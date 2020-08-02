module Command.Args where

type DispatchCall = [String] -> IO ()
type Dispatcher = [(String, DispatchCall)]
type Arguments = [String]

usage :: [String] -> IO ()
usage _ = putStrLn "usage: command [filename] [arguments]"

arguments :: Dispatcher -> Arguments -> Maybe (DispatchCall, Arguments)
arguments _ [] = Nothing
arguments dispatch (command:args) = lookup command dispatch >>= \x -> pure (x, args)
