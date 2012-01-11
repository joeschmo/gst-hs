module GstError where

import Control.Monad.Error
import GstTypes
import Text.ParserCombinators.Parsec

data GstError = TypeMismatch Typ Typ Exp
              | Parser ParseError
              | UnboundVar String Var
              | Default String

showError :: GstError -> String
showError (TypeMismatch t1 t2 e) = "Invalid type: expected "++(show t1)
                                    ++" instead found "++(show t2)++" in "++(show e)
showError (Parser err) = "Parse error at " ++ show err
showError (UnboundVar message var) = message ++ ": " ++ var
showError (Default s) = s

instance Show GstError where show = showError

instance Error GstError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either GstError

trapError action = catchError action (return . show)

extractVal :: ThrowsError a -> a
extractVal (Right val) = val

