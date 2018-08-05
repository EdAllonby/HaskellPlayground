module FunctionWithWhere where

printInc n = print plusTwo
    where plusTwo = n + 2

mult1 = x * y
    where x = 5
          y = 6

-- let x = 3; y = 1000 in x * 3 + y
where1 = x * 3 + y
    where x = 3
          y = 1000

-- let y = 10; x = 10 * 5 + y in x * 5
where2 = x * 5
    where x = 10 * 5 + y
          y = 10
          
-- let x = 7; y = negate x; z = y * 10 in z / x + y
where3 = z / x + y
    where x = 7
          y = negate x
          z = y * 10