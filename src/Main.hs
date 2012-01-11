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
import GstEnv


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

runRepl = nullEnv >>= until_ (readPrompt "gst>>> ") . evalAndPrint

readPrompt prompt = hFlush stdout >> readline prompt

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM showRes $ (liftThrows $ readExp expr) >>= (\exp -> liftM2 (,) (eval env exp) (typeof env emptyCtx exp)) 
    where showRes (e, t) = show e ++ " : " ++ show t

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

main :: IO ()
main = runRepl
