-- This is how you could implement a list data structure.
data RecursiveList a = Empty | Cons a (RecursiveList a) deriving (Show, Read, Eq, Ord) 
createdList =  5 `Cons` (4 `Cons` (5 `Cons` Empty))

-- The 5 is fixity, or how tightly it binds (affects if you need parentheses - i.e. 5 + 2 * 2 != (5 + 2) * 2 because * has a higher fixity than + (7 > 5))
infixr 5 :-:
data RecursiveListWithInfix a = Empty2 | a :-: (RecursiveListWithInfix a) deriving (Show, Read, Eq, Ord)
createdListInfix = 5 :-: 4 :-: 5 :-: Empty2
createListInfix2 = 3 :-: 2 :-: Empty2

infixr 5 ^++
(^++) :: RecursiveListWithInfix a -> RecursiveListWithInfix a -> RecursiveListWithInfix a
Empty2 ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

recursiveListsAdded = createdListInfix ^++ createListInfix2
