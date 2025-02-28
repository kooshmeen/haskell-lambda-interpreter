module Default where

import Lambda
import Binding

-- Variables (for convenience)
vx = Var "x"
vy = Var "y"
vz = Var "z"
vf = Var "f"
vg = Var "g"
vh = Var "h"
vm = Var "m"
vn = Var "n"

-- Basic combinators
m = Abs "x" $ App vx vx
i = Abs "x" $ vx
k = Abs "x" $ Abs "y" $ vx
ki = Abs "x" $ Abs "y" $ vy
c = Abs "x" $ Abs "y" $ Abs "z" $ App (App vx vz) vy
y = Abs "f" $ App fix fix
  where fix = Abs "x" $ App vf (App vx vx)

-- 4.1. Boolean encodings
bTrue = Abs "x" (Abs "y" vx)                              -- λx.λy.x
bFalse = Abs "x" (Abs "y" vy)                             -- λx.λy.y
bAnd = Abs "x" (Abs "y" (App (App vx vy) bFalse))         -- λx.λy.((x y) FALSE)
bOr = Abs "x" (Abs "y" (App (App vx bTrue) vy))           -- λx.λy.((x TRUE) y)
bNot = Abs "x" (App (App vx bFalse) bTrue)                -- λx.((x FALSE) TRUE)
bXor = Abs "x" (Abs "y" (App (App vx (App bNot vy)) vy))  -- λx.λy.((x (NOT y)) y)

-- 4.2. Pair encodings
pair = Abs "x" (Abs "y" (Abs "z" (App (App vz vx) vy)))   -- λx.λy.λz.((z x) y))
first = Abs "x" (App vx bTrue)                            -- λx.(x TRUE)
second = Abs "x" (App vx bFalse)                          -- λx.(x FALSE)

-- 4.3. Natural number encodings
n0 = Abs "f" (Abs "x" vx)                                                                                              -- λf.λx.x
n1 = Abs "f" (Abs "x" (App vf vx))                                                                                     -- λf.λx.(f x)
n2 = Abs "f" (Abs "x" (App vf (App vf vx)))                                                                            -- λf.λx.(f (f x))
nSucc = Abs "n" (Abs "f" (Abs "x" (App vf (App (App vn vf) vx))))                                                      -- λn.λf.λx.(f ((n f) x))

nAdd = Abs "n" (Abs "m" (Abs "f" (Abs "x" (App (App vn vf) (App (App vm vf) vx)))))                                    -- λn.λm.λf.λx.((n f) ((m f) x))
nMult = Abs "n" (Abs "m" (Abs "f" (App vn (App vm vf))))                                                               -- λn.λm.λf.(n (m f))

nPred = Abs "n" (Abs "f" (Abs "x" (App(App(App vn(Abs "g" (Abs "h" (App vh (App vg vf)))))(Abs "m" vx))(Abs "m" vm)))) -- λn.λf.λx.(((n (λg.λh.(h (g f))) (λm.x)) (λm.m)       

nSub = Abs "n" (Abs "m" (App (App vm nPred) vn))                                                                       -- λn.λm.((m PRED) n)

-- Default Context
defaultContext :: Context
defaultContext = 
    [ ("M", m)
    , ("I", i)
    , ("K", k)
    , ("KI", ki)
    , ("C", c)
    , ("Y", y)
    , ("TRUE", bTrue)
    , ("FALSE", bFalse)
    , ("AND", bAnd)
    , ("OR", bOr)
    , ("NOT", bNot)
    , ("XOR", bXor)
    , ("PAIR", pair)
    , ("FST", first)
    , ("SND", second)
    , ("N0", n0)
    , ("N1", n1)
    , ("N2", n2)
    , ("SUCC", nSucc)
    , ("PRED", nPred)
    ,("ADD", nAdd)
    , ("SUB", nSub)
    , ("MULT", nMult)
    ]
