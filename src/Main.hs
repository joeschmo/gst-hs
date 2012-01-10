module Main where
import System
import System.Console.Readline
import System.Exit
import System.IO

import Data.IORef
import Control.Monad

import GstParser
import GstEval
import GstTypes
import GstError


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

runRepl = until_ (readPrompt "gst>>> ") evalAndPrint

readPrompt prompt = hFlush stdout >> readline prompt

evalString :: String -> IO String
evalString expr = return $ extractVal $ trapError 
    (do
        exp <- readExp expr
        t <- typeof emptyCtx exp
        e <- eval exp
        return $ (show e) ++ " : " ++ (show t))

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

main :: IO ()
main = runRepl
