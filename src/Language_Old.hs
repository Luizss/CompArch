module Language_Old where

type Var   = String
type Label = String
type Mem   = String
type Addr  = String

-- Statement
data S = RM Var Mem Addr S
       | WM Mem Addr Var S
       | IF Var S S
       | LA Label -- Label
       | LO Label S
       | END
       deriving (Show, Eq)

test :: S
test = LO "LOOP" $
       RM "x1" "a" "y" $
       IF "x1" (WM "x2" "x1" "1" END) $
       RM "x1" "a" "y" $
       LA "LOOP"

test' :: IO ()
test' = putStrLn $ prettyShow test

prettyShow :: S -> String
prettyShow = prettyShowTab 0

prettyShowTab :: Int -> S -> String
prettyShowTab n statement = concat (replicate n "   ") ++ case statement of
  RM var mem addr cont -> var ++ " <- " ++ mem ++ "[" ++ addr ++ "]" ++ "\n" ++ go n cont
  WM mem addr var cont -> mem ++ "[" ++ addr ++ "] <- " ++ var ++ "\n" ++ go n cont
  IF var branch cont -> "if " ++ var ++ ":\n" ++ go (n+1) branch ++ "\n" ++ go n cont
  LA label -> label
  LO label cont -> label ++ ":\n" ++ go (n+1) cont
  END -> "end"
  where go = prettyShowTab
