{- darts Senán d'Art -}
module Ex02 where
import Data.List ((\\))

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !

type Id = String

data Expr
  = Val Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
  | Var Id
  | Def Id Expr Expr
  deriving (Eq, Show)

type Dict k d  =  [(k,d)]

define :: Dict k d -> k -> d -> Dict k d
define d s v = (s,v):d

find :: Eq k => Dict k d -> k -> Maybe d
find []             _                 =  Nothing
find ( (s,v) : ds ) name | name == s  =  Just v
                         | otherwise  =  find ds name

type EDict = Dict String Double

v42 = Val 42 ; j42 = Just v42

-- Part 1 : Evaluating Expressions -- (60 test marks, worth 15 Exercise Marks) -

-- Implement the following function so all 'eval' tests pass.

-- eval should return Nothing if:
  -- (1) a divide by zero operation was going to be performed;
  -- (2) the expression contains a variable not in the dictionary.



eval :: EDict -> Expr -> Maybe Double
eval d (Var e) = find d e
eval _ (Val e) = Just e
eval _ (Dvd e (Val 0)) = Nothing 

eval d (Add a b) 
  = case (eval d a, eval d b) of
    (Just x, Just y) -> Just (x+y)
    _                -> Nothing 

eval d (Mul a b) 
  = case (eval d a, eval d b) of
    (Just x, Just y) -> Just (x*y)
    _                -> Nothing 

eval d (Sub a b) 
  = case (eval d a, eval d b) of
    (Just x, Just y) -> Just (x-y)
    _                -> Nothing 

eval d (Dvd a b) 
  = case (eval d a, eval d b) of
    (Just x, Just y) -> Just (x/y)
    _                -> Nothing 

-- eval d (Def n a b) = (define d n (eval d a)), eval d b

eval d (Def n a b)
  = let x = eval d a
  in case (x) of 
    Just x -> eval (define d n x) b
    Nothing -> error "asdf"

eval d e = Just 1e-99




-- Part 1 : Expression Laws -- (15 test marks, worth 15 Exercise Marks) --------

{-

There are many, many laws of algebra that apply to our expressions, e.g.,

  x + y            =  y + z         Law 1
  x + (y + z)      =  (x + y) + z   Law 2
  x - (y + z)      =  (x - y) - z   Law 3
  (x + y)*(x - y)  =  x*x - y*y     Law 4
  ...

  We can implement these directly in Haskell using Expr

  Function LawN takes an expression:
    If it matches the "shape" of the law lefthand-side,
    it replaces it with the corresponding righthand "shape".
    If it does not match, it returns Nothing

    Implement Laws 1 through 4 above
-}


law1 :: Expr -> Maybe Expr
law1 (Add x y) = Just (Add y x)

law2 :: Expr -> Maybe Expr
law2 (Add x (Add y z)) = Just (Add (Add x y) z)

law3 :: Expr -> Maybe Expr
law3 (Sub x (Add y z)) = Just (Sub (Sub x y) z) 

law4 :: Expr -> Maybe Expr
law4 e = j42
-- law4 (Mul (Add x y) (Sub x y)) = Just (Sub (Mul x x) (Mul y y))
