module GstParser (parseTyp, parseExp, readExp, readExpList) where
import GstTypes
import GstError
import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Data.Char

-- Type parser

sspace = oneOf " \t"

parseTyp :: Parser Typ
parseTyp = try parseArr
       <|> parseParenT
       <|> parseNat

parseNat :: Parser Typ
parseNat = string "nat" >> return Nat

parseArr :: Parser Typ
parseArr = do
    t1 <- (parseParenT <|> parseNat)
    many sspace
    string "->"
    many sspace
    t2 <- parseTyp
    return $ Arr t1 t2

parseParenT :: Parser Typ
parseParenT = do
    char '('
    t <- parseTyp
    char ')'
    return t

-- Expression parser

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "gst" input of
    Left err -> throwError $ Parser err
    Right exp -> return exp

readExp = readOrThrow parseExp
readExpList = readOrThrow (endBy parseExp (many1 $ char '\n'))

parseExpr = try parseS
    <|> try parseLam
    <|> try parseNatrec
    <|> try parseParenE
    <|> try parseZ
    <|> parseVar

parseExp = try parseLoad <|> try parseSet <|> parseAp

parseLoad :: Parser Exp
parseLoad = do
    string "load"
    many1 sspace
    char '\"'
    f <- many1 (satisfy (/= '\"'))
    char '\"'
    return $ Load f

parseAp :: Parser Exp
parseAp = do 
    e1 <- parseExpr
    many sspace
    es <- sepEndBy parseParenE (many sspace)
    return $ foldr (\e -> (\e' -> Ap e' e)) e1 (reverse es)

parseSet :: Parser Exp
parseSet = do
    v <- parseVarname
    many sspace
    char '='
    many sspace
    e <- parseAp
    return $ Set v e

parseParenE :: Parser Exp
parseParenE = do
    char '('
    many sspace
    e <- parseAp
    many sspace
    char ')'
    return e

parseZ :: Parser Exp
parseZ = do
    z <- parseVarname
    if length z == 1 then
        (case head z == 'z' of
              True  -> return Z
              False -> return $ X z)
    else return $ X z

parseS :: Parser Exp
parseS = do
    string "s("
    e <- parseExp
    char ')'
    return $ S e

parseVarname :: Parser String
parseVarname = do
    first <- (lower <|> upper)
    rest <- many alphaNum
    return $ first:rest

parseVar :: Parser Exp
parseVar = do
    v <- parseVarname
    return $ X v

parseLam :: Parser Exp
parseLam = do
    string "fn"
    many sspace
    char '('
    many sspace
    v <- parseVarname
    many sspace
    char ':'
    many sspace
    t <- parseTyp
    many sspace
    char ')'
    many sspace
    e <- parseExp
    return $ Lam t v e

parseNatrec :: Parser Exp
parseNatrec = do
    string "natrec"
    many1 sspace
    e <- parseExp
    many sspace
    char '{'
    many sspace
    char 'z'
    many sspace
    string "=>"
    many sspace
    e0 <- parseAp
    many sspace
    char '|'
    many sspace
    string "s("
    x <- parseVarname
    char ')'
    many1 sspace
    string "with"
    many1 sspace
    y <- parseVarname
    many sspace
    string "=>"
    many sspace
    e1 <- parseAp
    many sspace
    char '}'
    return $ Natrec e e0 x y e1

