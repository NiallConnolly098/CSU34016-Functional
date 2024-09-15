module Ex2 where

add :: Int -> Int -> Int
add x y = (x+y) `mod` 65563

mul :: Int -> Int -> Int
mul x y
  | p == 0    = 1
  | otherwise = p
  where p = (x*y) `mod` 65563

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (3 marks)
f1 :: [a] -> [a]
-- returns a list of every 115th element of its input
f1 [] = []
f1 ns = go ns 1
  where
    go [] _ = []
    go (y:ys) n
      | n `mod` 115 == 0 = y : go ys (n + 1)
      | otherwise = go ys (n + 1)

-- Q2 (3 marks)
f2 :: [Int] -> Int
-- sums every 255th element of its input
f2 ns = sum [n | (n, index) <- zip ns [1..], index `mod` 255 == 0]


-- Q3 (4 marks)
f3 :: [Int] -> Int
-- multiplies every 395th element of its input
f3 ns = go ns 1 1
  where
    go [] _ product = product
    go (y:ys) n product
      | n `mod` 395 == 0 = go ys (n + 1) (product * y)
      | otherwise = go ys (n + 1) product

-- Q4 (8 marks)
f4 mis = processOperations mis 0
processOperations :: [Maybe Int] -> Int -> (Int,[Maybe Int])
processOperations [] result = (result, [])
processOperations (Just opcode : rest) result =
  case opcode of
-- Operation Table (See Exercise2 description on BB)
--    ___________________________________________
--    | opcode | operation | operands | Nothing |
--    -------------------------------------------
--    |   48   |    add    | fixed 4  | term    |
--    |   92   |    add    | fixed 3  | skip    |
--    |   29   |    add    | fixed 3  | 0       |
--    |   64   |    add    | stop@ 3  | term    |
--    |   89   |    add    | stop@ 3  | skip    |
--    |   13   |    add    | stop@ 4  | 5       |
--    |   10   |    mul    | fixed 6  | term    |
--    |   98   |    mul    | fixed 6  | skip    |
--    |   22   |    mul    | fixed 4  | 9       |
--    |   63   |    mul    | stop@ 6  | term    |
--    |   43   |    mul    | stop@ 3  | skip    |
--    |   16   |    mul    | stop@ 5  | 8       |
--    -------------------------------------------
    48 -> processFixedOperation rest 4 result add
    92 -> processFixedOperation rest 3 result add
    29 -> processFixedOperation rest 3 result (addWithReplacement 0)
    64 -> processStopOperation rest 3 result add
    89 -> processStopOperation rest 3 result add
    13 -> processStopOperation rest 4 result (addWithReplacement 5)
    10 -> processFixedOperation rest 6 result mul
    98 -> processFixedOperation rest 6 result mul
    22 -> processFixedOperation rest 4 result (mulWithReplacement 9)
    63 -> processStopOperation rest 6 result mul
    43 -> processStopOperation rest 3 result mul
    16 -> processStopOperation rest 5 result (mulWithReplacement 8)
    _ -> processOperations rest result
processOperations (Nothing : rest) result = processOperations rest result

processFixedOperation :: [Maybe Int] -> Int -> Int -> (Int -> Int -> Int) -> (Int, [Maybe Int])
processFixedOperation ops n result operation
    | length ops >= n =
      let (operands, remaining) = splitAt n ops
          processed = foldl (\acc x -> maybe 0 id x) 1 operands
          newResult = operation result processed
      in processOperations remaining newResult
    | otherwise = (result, ops)

processStopOperation :: [Maybe Int] -> Int -> Int -> (Int -> Int -> Int) -> (Int, [Maybe Int])
processStopOperation ops stop result operation
    | length ops >= stop =
      let (operands, remaining) = splitAt stop ops
          processed = foldl (\acc x -> maybe 0 id x) 1 operands
          newResult = operation result processed
      in (newResult, remaining)
    | otherwise = (result, ops)

addWithReplacement :: Int -> Int -> Int -> Int
addWithReplacement result operand replacement
    | operand == 0 = replacement
    | otherwise = result + operand

mulWithReplacement :: Int -> Int -> Int -> Int
mulWithReplacement result operand replacement
    | operand == 9 = replacement
    | otherwise = result * operand


-- Q5 (2 marks)
-- uses f4 to process all the opcodes in the maybe list,
-- by repeatedly applying it to the leftover part
f5 mis = processOpcodes mis []
processOpcodes :: [Maybe Int] -> [Int] -> [Int]
processOpcodes [] results = reverse results
processOpcodes mis results =
    let (result, remaining) = f4 mis
    in processOpcodes remaining (result : results)
-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

