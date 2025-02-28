module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.

-- inlocuieste macro-urile dintr-o expr lambda fie cu expr lambda, fie cu un string (in cazul in care nu exista)
-- daca e doar o variabila in context se intoarce direct
-- daca e apl se inlocuiesc recursiv
-- daca e abstractie se inlocuieste recursiv
-- daca e macro se uita in dictionar si daca exista se inlocuieste recursiv
-- altfel se intoarce "eroare"
substituteMacros :: Context -> Lambda -> Either String Lambda
substituteMacros ctx (Var x) = Right (Var x)
substituteMacros ctx (App e1 e2) = do
    e1' <- substituteMacros ctx e1
    e2' <- substituteMacros ctx e2
    return (App e1' e2')
substituteMacros ctx (Abs x e) = do
    e' <- substituteMacros ctx e
    return (Abs x e')
substituteMacros ctx (Macro x) = case lookup x ctx of
    Just e -> substituteMacros ctx e
    Nothing -> Left ("Macro " ++ x ++ " not found")

-- simplifica din context o expresie lambda, pas cu pas
-- mai intai se inlocuiesc macro-urile cu expresii lambda, daca e cazul
-- apoi se returneaza pasul simplificat
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step expr = do
    substitutedExpr <- substituteMacros ctx expr
    return (simplify step substitutedExpr)

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
