module GstEnv where

import Data.IORef
import Control.Monad
import Control.Monad.Error

import GstTypes
import GstError

type Env = IORef [(Var, IORef Exp)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT GstError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractVal

isBound :: Env -> Var -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> Var -> IOThrowsError Exp
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> Var -> Exp -> IOThrowsError Exp
setVar envRef var exp = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable " var)
          (liftIO . (flip writeIORef exp))
          (lookup var env)
    return exp

defineVar :: Env -> Var -> Exp -> IOThrowsError Exp
defineVar envRef var exp = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
       then setVar envRef var exp >> return exp
       else liftIO $ do
           valueRef <- newIORef exp
           env <- readIORef envRef
           writeIORef envRef ((var, valueRef) : env)
           return exp
