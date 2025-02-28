{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser (parseLambda, parseLine) where

import Control.Applicative
import Data.Char
import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f mp =
        do
            x <- mp
            return $ f x

instance Applicative Parser where
    af <*> mp =
        do
            f <- af
            v <- mp
            return $ f v
    pure = return

instance Monad Parser where
    return v = Parser $ \s -> Just (v, s)
    mp >>= f = Parser $ \s -> case parse mp s of
        Nothing -> Nothing
        Just (v, rest) -> parse (f v) rest

-- parser care nu intoarce nimic
failParser :: Parser a
failParser = Parser $ \s -> Nothing

instance Alternative Parser where
    empty = failParser
    p1 <|> p2 = Parser $ \s -> case parse p1 s of
        Nothing -> parse p2 s
        Just (v, rest) -> Just (v, rest)

-- parser pentru un caracter anume
charParser :: Char -> Parser Char
charParser c = Parser $ \s ->
    case s of
        [] -> Nothing
        (x:xs) -> if x == c then Just (x, xs) else Nothing

-- parser pentru un caracter care satisface o conditie
predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s ->
    case s of
        [] -> Nothing
        (x:xs) -> if p x then Just (x, xs) else Nothing

-- parser pt macro-uri
-- se foloseste de predicateParser pentru a verifica daca un caracter este specific macro
macroParser :: Parser String
macroParser = do
    x <- predicateParser isUpperOrDigit
    xs <- starParser (predicateParser isUpperOrDigit)
    return (x:xs)

-- parser pt variabile
-- variabile = litere
varParser :: Parser String
varParser = do
    x <- predicateParser isAlpha
    xs <- starParser (predicateParser isAlpha)
    return (x:xs)

-- aplica un parser de mai multe ori
plusParser :: Parser a -> Parser [a]
plusParser p = do
    x <- p
    xs <- starParser p
    return (x:xs)

-- aplica un parser de mai multe ori
starParser :: Parser a -> Parser [a]
starParser p = plusParser p <|> return []

-- parser pt spatii
whitespaceParser :: Parser String
whitespaceParser = starParser (charParser ' ')

-- verifica daca un caracter este litera mare sau cifra => macro
isUpperOrDigit :: Char -> Bool
isUpperOrDigit c = isUpper c || isDigit c

-- verifica daca un string este macro
-- aplicand functia de mai sus pe primul caracter din string
-- macro = litere mari si/sau cifre
isMacro :: String -> Bool
isMacro var = isUpperOrDigit (head var)

-- parser pt expresie lambda, intoarce variabila daca nu contine macro,
-- altfel intoarce macro
lambdaVar :: Parser Lambda
lambdaVar = do
    var <- varParser <|> macroParser
    if isMacro(var)
        then return (Macro var)
        else return (Var var)

-- parser pt Abs
-- abs inseamna lambda (\\)variabila.expr
abstraction :: Parser Lambda
abstraction = do
    charParser '\\'
    var <- varParser
    charParser '.'
    body <- lambda
    return (Abs var body)

-- parser pt App
-- app inseamna (expr1 expr2)
application :: Parser Lambda
application = do
    charParser '('
    func <- lambda
    whitespaceParser
    arg <- lambda
    charParser ')'
    return (App func arg)

-- parser pt expresie lambda
-- poate fi o variabila, o abstractie sau o aplicatie
lambda :: Parser Lambda
lambda = lambdaVar <|> abstraction <|> application

-- 2.1./3.2.
-- parseaza un lambda
-- daca rezulta un Just, farra rest de string, atunci intoarce lambda-ul
-- altfel, eroare
parseLambda :: String -> Lambda
parseLambda s = case parse lambda s of
    Just (result, "") -> result
    _ -> error "Parsing error"

-- 3.3.
-- parseaza o linie
-- daca rezulta un Just, fara rest de string, atunci intoarce linia
-- altfel, "eroare"
parseLine :: String -> Either String Line
parseLine s = case parse lineParser s of
    Just (result, "") -> Right result
    _ -> Left "Parsing error"

-- parser pt linie, care poate fi binding sau doar expresie
lineParser :: Parser Line
lineParser = parseBinding <|> parseExpr

-- parseaza o expresie
parseExpr :: Parser Line
parseExpr = do
    expr <- lambda
    return (Eval expr)

-- parseaza un biding, adica variabila=expr
parseBinding :: Parser Line
parseBinding = do
    var <- varParser
    charParser '='
    expr <- lambda
    return (Binding var expr)
