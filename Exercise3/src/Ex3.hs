module Ex3 where

--required for all Qs:
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
find :: String -> Dict -> Maybe Float
find s [] = Nothing
find s ((t,f):d)
  | s==t       =  Just f
  | otherwise  =  find s d

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (8 marks)
-- implement the following function (which may have runtime errors):
eval :: Dict -> Xprsn -> Float
eval _ (Literal f) = f
eval dict (VName s) = case find s dict of
                        Just f -> f
                        Nothing -> error ("Variable not found: " ++ s)
eval dict (Ratio x y) = eval dict x / eval dict y
eval dict (Times x y) = eval dict x * eval dict y
eval dict (Negate x) = - (eval dict x)
eval dict (Not x) = if eval dict x == 0.0 then 1.0 else 0.0
eval dict (Equal x y) = if eval dict x == eval dict y then 1.0 else 0.0
eval dict (NotZero x) = if eval dict x /= 0.0 then 1.0 else 0.0

-- Q2 (8 marks)
-- implement the following function (which always returns a value):
meval :: Dict -> Xprsn -> Maybe Float
meval _ (Literal f) = Just f
meval dict (VName s) = find s dict
meval dict (Ratio x y) = case meval dict y of
  Just 0.0 -> Nothing
  Just fy -> fmap (/ fy) (meval dict x)
  Nothing -> Nothing
meval dict (Times x y) = case (meval dict x, meval dict y) of
  (Just vx, Just vy) -> Just (vx * vy)
  _ -> Nothing
meval dict (Negate x) = fmap negate (meval dict x)
meval dict (Not x) = fmap (\vx -> if vx == 0.0 then 1.0 else 0.0) (meval dict x)
meval dict (Equal x y) = case (meval dict x, meval dict y) of
  (Just vx, Just vy) -> Just (if vx == vy then 1.0 else 0.0)
  _ -> Nothing
meval dict (NotZero x) = fmap (\vx -> if vx /= 0.0 then 1.0 else 0.0) (meval dict x)

-- Q3 (4 marks)
-- Laws of Arithmetic for this question:
--    x + 0 = x
--    0 + x = x
--    x - 0 = x
--    x - x = 0
--    x * 0 = 0
--    1 * x = x
-- The following function should implement the two laws applicable
-- for *your* Xprsn datatype.
simp :: Xprsn -> Xprsn
simp (Times x (Literal 0.0)) = Literal 0.0
simp (Ratio x (Literal 1.0)) = simp x
simp x = x

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

