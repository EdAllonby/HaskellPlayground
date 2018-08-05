module ChapterExercises where

-- before == after
explicit1 = 2 + 2 * 3 - 1 == 2 + (2 * 3) - 1

explicit2 = b == a
    where b = (^) 10 $ 1 + 1
          a = 10 ^ (1 + 1)

explicit3 = b == a
    where b = 2 ^ 2 * 4 ^ 5 + 1
          a = 1 + ((2 ^ 2) * (4 ^ 5))

z = 7
y = z + 8
x = y ^ 2
waxOn = x * 5

triple x = x * 3
waxOnTriple = triple waxOn

waxOnRewritten = x * 5
    where z = 7
          y = z + 8
          x = y ^ 2

tripleWaxOnRewritten = triple waxOnRewritten

waxOff x = triple x

waxOffUpdated x = (^2) $ triple x 