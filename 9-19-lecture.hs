{-# LNAUGAGE GADTs #-}

data Exp where
  ELit :: Int -> Exp
  EAdd :: Exp -> Exp -> Exp
  ESub :: Exp -> Exp -> Exp
  EMul :: Exp -> Exp -> Exp
  EDiv :: Exp -> Exp -> Exp
  EMod :: Exp -> Exp -> Exp
  deriving (Eq, Show)

prettyPrint :: Exp -> String
prettyPrint (ELit n) = show n
prettyPrint (EAdd e1 e2) = "(" ++ (prettyPrint e1) ++ " + " ++ (prettyPrint e2) ++ ")"
prettyPrint (ESub e1 e2) = "(" ++ (prettyPrint e1) ++ " - " ++ (prettyPrint e2) ++ ")"
prettyPrint (EMul e1 e2) = "(" ++ (prettyPrint e1) ++ " * " ++ (prettyPrint e2) ++ ")"
prettyPrint (EDiv e1 e2) = "(" ++ (prettyPrint e1) ++ " / " ++ (prettyPrint e2) ++ ")"
prettyPrint (EMod e1 e2) = "(" ++ (prettyPrint e1) ++ " % " ++ (prettyPrint e2) ++ ")"

eval :: Exp -> Int
eval (ELit n) = n
eval (EAdd e1 e2) = (eval e1) + (eval e2)
eval (ESub e1 e2) = (eval e1) - (eval e2)
eval (EMul e1 e2) = (eval e1) * (eval e2)
eval (EDiv e1 e2) = (eval e1) `div` (eval e2)
eval (EMod e1 e2) = (eval e1) `mod` (eval e2)
