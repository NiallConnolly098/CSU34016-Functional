module Ex4 where

--required for Q1
data Xprsn -- the expression datatype
  = Literal Float -- floating-point value
  | VName String -- variable/identifier name
  | Ratio Xprsn Xprsn -- divide first by second
  | Times Xprsn Xprsn -- multiplies both
  | Negate Xprsn -- numerical negation (-x)
  -- the following are boolean expressions (using numbers)
  -- the number 0.0 represents False, all others represent True.
  | Not Xprsn -- logical not
  | Equal Xprsn Xprsn -- True if both are the same
  | NotZero Xprsn -- True if numeric value is non-zero
  deriving (Eq,Ord,Show)

type Dict = [(String,Float)]
insert :: String -> Float -> Dict -> Dict
insert s f d = (s,f):d
find :: MonadFail m => String -> Dict -> m Float
find s [] = fail (s++" not found")
find s ((t,f):d)
  | s==t       =  return f
  | otherwise  =  find s d

-- required for Q2
x `incfst` _  =  x + 1
_ `incsnd` y  =  1 + y
type Thing = ([Bool],Bool)

-- required for all Qs:

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (8 marks)
-- implement the following function (which always returns a value):
mdeval :: MonadFail m => Dict -> Xprsn -> m Float
mdeval d (Literal x) = return x
mdeval d (VName var) = find var d
mdeval d (Ratio expr1 expr2) = do
  val1 <- mdeval d expr1
  val2 <- mdeval d expr2
  if val2 == 0.0 then 
    fail "Division by zero" 
  else 
    return (val1 / val2)
mdeval d (Times expr1 expr2) = do
  val1 <- mdeval d expr1
  val2 <- mdeval d expr2
  return (val1 * val2)
mdeval d (Negate expr) = fmap negate (mdeval d expr)
mdeval d (Not expr) = do
  val <- mdeval d expr
  return (if val == 0.0 then 1.0 else 0.0)
mdeval d (Equal expr1 expr2) = do
  val1 <- mdeval d expr1
  val2 <- mdeval d expr2
  return $ if val1 == val2 then 1.0 else 0.0
mdeval d (NotZero expr) = fmap (\expr -> if expr == 0.0 then 0.0 else 1.0) (mdeval d expr)

-- Q2 (6 marks)
-- Consider the following four recursive pattern definitions:
len :: Int -> [Int] -> Int
len z []     = z
len z (x:xs) = x `incsnd` (len z xs)
sumup :: Int -> [Int] -> Int
sumup sbase []     = sbase
sumup sbase (n:ns) = n + (sumup sbase ns)
prod :: Int -> [Int] -> Int
prod mbase []     = mbase
prod mbase (n:ns) = n * (prod mbase ns)
cat :: [Thing] -> [[Thing]] -> [Thing]
cat pfx []     = pfx
cat pfx (xs:xss) = xs ++ (cat pfx xss)

-- They all have the same abstract pattern,
-- as captured by the following Higher Order Function (HOF):
foldR z _ [] = z
foldR z op (x:xs) = x `op` foldR z op xs

-- We can gather the `z` and `opr` arguments into a tuple: (op,z)
-- which allows us to construct a call to foldR as:
dofold (op,z) = foldR z op

-- Your task is to complete the tuples below,
-- so that `dofold` can be used to implement the fns. above.

-- dofold lenTuple = len
lenTuple :: (Int -> Int -> Int,Int)
lenTuple = (incsnd, 0)

-- dofold sumupTuple = sumup
sumupTuple :: (Int -> Int -> Int,Int)
sumupTuple = ((+), 0)

-- dofold prodTuple = prod
prodTuple :: (Int -> Int -> Int,Int)
prodTuple = ((*), 1)

-- dofold catTuple = cat
catTuple :: ([Thing] -> [Thing] -> [Thing],[Thing])
catTuple = ((++), [])

-- Q3 (11 marks)
sub = subtract -- shorter!
ops = [(+28),(+27),(+25),(+27),(*24),(*22),(31-),(25-),(*19)]

-- (!) This question requires modifying Main.hs
-- See, and/or compile and run Main.hs for further details

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

applyOps :: [Int -> Int] -> [Int] -> [Int]
applyOps [] _ = []
applyOps _ [] = []
applyOps (f:fs) (x:xs) = f x : applyOps (fs ++ [f]) xs