module Main where

data Expr = EInt Int
          | EAdd Expr Expr | EMinus Expr Expr
          | EMult Expr Expr | EDiv Expr Expr | ERem Expr Expr
          deriving (Read)

eval :: Expr -> Maybe Int
eval (EInt i) = Just i
eval (EAdd e1 e2) = pure (+) <*> (eval e1) <*> (eval e2)
eval (EMinus e1 e2) = pure (-) <*> (eval e1) <*> (eval e2)
eval (EMult e1 e2) = pure (*) <*> (eval e1) <*> (eval e2)
eval (EDiv e1 e2) = case (eval e2) of
  Just 0 -> Nothing
  v2 -> pure (div) <*> (eval e1) <*> v2
eval (ERem e1 e2) = case (eval e2) of
  Just 0 -> Nothing
  v2 -> pure (rem) <*> (eval e1) <*> v2

main :: IO ()
main = do
  x <- getLine
  putStrLn . show . eval . read $ x
