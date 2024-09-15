module Ex5 where
-- no code for Q1


-- for Q2:
frec x  =  if x <= 4 then 4 else x * frec (x `div` 5)



-- for Q3:
bonus []      =  12
bonus (x:xs)  =  x + 9 + bonus xs




--for Q4:
casef x
  | x < 7   =  2*x
  | x >= 7  = 2*x-1


