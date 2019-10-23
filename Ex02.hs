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
eval d (Var e) = find d e --find the Var e in the dict d
eval _ (Val e) = Just e --no need to find Val in dict, just return it
eval _ (Dvd e (Val 0)) = Nothing --prevent divide by zero 

eval d (Add a b) 
  = case (eval d a, eval d b) of  --evaluate both expressions for pattern match
    (Just x, Just y) -> Just (x+y) --if both expressions are valid, return the sum
    _                -> Nothing 

eval d (Mul a b) 
  = case (eval d a, eval d b) of  --evaluate both expressions for pattern match
    (Just x, Just y) -> Just (x*y) --if both expressions are valid, return the product
    _                -> Nothing 

eval d (Sub a b) 
  = case (eval d a, eval d b) of  --evaluate both expressions for pattern match
    (Just x, Just y) -> Just (x-y) --if both expressions are valid, return the difference
    _                -> Nothing 

eval d (Dvd a b) 
  = case (eval d a, eval d b) of  --evaluate both expressions for pattern match
    (Just x, Just y) -> Just (x/y) --if both expressions are valid, return the div
    _                -> Nothing 

eval d (Def n a b)
  = let x = eval d a --evaluate expr a so we can use the result for b
  in case (x) of --check what's up with the result of expr a
    Just x -> eval (define d n x) b --add the result of expr a to the dictionary and then use that new dict for expr b
    _      -> Nothing

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
law1 thing 
    = case thing of --if it's real
      (Add x y) -> Just (Add y x) --just swap
      _         -> Nothing

law2 :: Expr -> Maybe Expr
law2 thing
    = case thing of --if it's real
      (Add x (Add y z)) -> Just (Add (Add x y) z) --just swap
      _                 -> Nothing

law3 :: Expr -> Maybe Expr
law3 thing
    = case thing of --if it's real
      (Sub x (Add y z)) -> Just (Sub (Sub x y) z) --just swap
      _                 -> Nothing


law4 :: Expr -> Maybe Expr
law4 thing  
    =case (thing) of --if it's real
      (Mul (Add a b) (Sub c d)) -> law4hlp (a==c) (b==d) (Sub (Mul a c) (Mul b d)) --call the helper function to eval the bools and return the right thing
      _                                     -> Nothing
      
law4hlp :: Bool -> Bool -> Expr -> Maybe Expr
law4hlp a b c
        =case (a, b) of 
          (True, True) -> Just c --if a==c and b==d, return the new function
          (_, _)       -> Nothing
