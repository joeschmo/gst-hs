module Main where
import System
import System.Console.Readline
import System.Exit
import System.IO

import Data.IORef

import GstParser
import GstEval
import GstTypes

parseAndEval :: String -> IO ()
parseAndEval exp =
    case readExp exp of
         Left err -> putStrLn $ "syntax error: " ++ show err
         Right e -> evalWith e

flushStr str = putStr str >> hFlush stdout

flushStrLn str = putStrLn str >> hFlush stdout

until_ prompt action = do
    result <- prompt
    case result of
         Nothing -> exitSuccess
         Just line -> case line of
                        "" -> return ()
                        _  -> do addHistory line
                                 action line
    until_ prompt action

runRepl = until_ (readPrompt "gst>>> ") parseAndEval

readPrompt prompt = hFlush stderr >> hFlush stdout >> readline prompt

main :: IO ()
main = runRepl
