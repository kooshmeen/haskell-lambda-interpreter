module Lambda where

import Data.List (nub, (\\))

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || not (elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
-- ia variabilele dintr-o expresie lambda:
-- daca e variabila, pune doar variabila
-- daca e apl intre 2 expresii, pune variabilele din ambele
-- daca e abstrractizare, pune variabila si variabilele din corp (verificand duplicate)
vars :: Lambda -> [String]
vars (Var x) = [x]
vars (App e1 e2) = vars e1 ++ vars e2
vars (Abs x e) = nub (x : vars e)

-- 1.2.
-- ia variabilele libere dintr-o expresie lambda:
-- daca e variabila, pune doar variabila
-- daca e apl intre 2 expresii, pune variabilele libere din ambele (verif dupl)
-- daca e abs, pune variabilele libere din corp (verif duplicate)
freeVars :: Lambda -> [String]
freeVars (Var x) = [x]
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (Abs x e) = filter (/= x) (freeVars e)

-- 1.3.
-- gaseste prima variabila care nu exista deja
-- luand literele de la a la z, apoi punand un sufix de la a la z
newVar :: [String] -> String
newVar = findNewVar potential
  where
    potential = [[c] | c <- ['a'..'z']] ++ [c : extra | extra <- potential, c <- ['a'..'z']]

-- gaseste recursiv prima variabila care nu exista deja
findNewVar :: [String] -> [String] -> String
findNewVar (x:xs) ys
  | isMember x ys = findNewVar xs ys
  | otherwise = x

-- verifica recursvi daca un element exista
isMember :: String -> [String] -> Bool
isMember _ [] = False
isMember x (y:ys)
  | x == y = True
  | otherwise = isMember x ys

-- 1.4.
-- verifica daca o expresie lambda e in forma normala
-- daca e variabila, e in forma normala
-- daca e abs, e in forma normala daca corpul e in forma normala
-- daca e apl din abs si o expresie, nu e in forma normala, se poate reduce
-- daca e apl, e in forma normala daca ambele expresii sunt in forma normala
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (Abs _ e) = isNormalForm e
isNormalForm (App (Abs _ _) _) = False
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2

-- 1.5.
-- inlocuieste variabila x din e_1 cu e_2
-- foloseste functia recursiva replace, care face inlocuirea efectiva in fct de tipul subexpresiei:
-- daca gaseste y egal cu x, il inlocuieste cu e_2, altfel ramane la fel
-- in apl, face apl pe replace la cele 2 subexpresii
-- in abs, daca y e x, ramane la fel
-- daca y e var libera in e_2, nu putem sa o luam ptc am caputra-o, deci facem alta variabila y' care nu exista in vars e si vars e_2
-- si o inlocuim pe y cu y' in e
-- altfel, face abs pe y si replace in e
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e_1 e_2 = replace e_1
  where
    replace (Var y)
      | y == x = e_2
      | otherwise = Var y
    replace (App e1 e2) = App (replace e1) (replace e2)
    replace (Abs y e)
      | y == x = Abs y e
      | y `isMember` freeVars e_2 = Abs y' (replace e')
      | otherwise = Abs y (replace e)
      where
        y' = newVar (vars e ++ vars e_2)
        e' = substitute y (Var y') e

--functie aux pt reduce;
-- inlocuieste variabila y cu e' in e
-- similar cu reduce, doar ca la abs nu mai verifica daca e variabila libera
-- ptc functia e apelata doar cu o variabila noua
substitute :: String -> Lambda -> Lambda -> Lambda
substitute y e (Var z)
  | z == y = e
  | otherwise = Var z
substitute y e (App e1 e2) = App (substitute y e e1) (substitute y e e2)
substitute y e (Abs z e')
  | z == y = Abs z e
  | otherwise = Abs z (substitute y e e')

-- 1.6.
-- daca e doar o expresie, nu face nimic
-- daca e abs, face abs recursiv pt expr
-- daca e apl, si prima expr e in forma normala, face apl pr prima si recursiv pt a doua
-- altfel fare apl recursiv pt prima si a doua
-- daca e apl de abs si o expresie, face reduce
normalStep :: Lambda -> Lambda
normalStep (App (Abs x e1) e2) = reduce x e1 e2
normalStep (App e1 e2)
  | isNormalForm e1 = App e1 (normalStep e2)
  | otherwise = App (normalStep e1) e2
normalStep (Abs x e) = Abs x (normalStep e)
normalStep e = e

-- 1.7.
-- daca e doar o expresie, nu face nimic
-- daca e abs, face abs recursiv pt expr
-- daca e apl si ambele sunt in forma normala, daca prima e abs, face reduce
-- altfel ramane la fel
applicativeStep :: Lambda -> Lambda
applicativeStep (App e1 e2)
  | isNormalForm e1 && isNormalForm e2 = case e1 of
    Abs x e -> reduce x e e2
    _ -> App e1 e2
  | isNormalForm e1 = App e1 (applicativeStep e2)
  | otherwise = App (applicativeStep e1) e2
applicativeStep (Abs x e) = Abs x (applicativeStep e)
applicativeStep e = e

-- 1.8.
-- simplifica o expresie lambda
-- daca e in forma normala, returneaza lista vida (adica lista cu pasii acumulati)
-- altfel, simplifica recursiv si adauga la lista
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step e = e : if isNormalForm e
                      then []
                      else simplify step (step e)

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
