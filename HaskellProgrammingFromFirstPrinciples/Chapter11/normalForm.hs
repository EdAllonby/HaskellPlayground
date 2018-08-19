data Fiction = Fiction deriving Show

data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show

-- Alias
type AuthorName = String

-- This isn't a sum of products, so is not in normal form.
data Author = Author (AuthorName, BookType)

-- This is now in normal form.
data Author' =
    Fiction' AuthorName
  | Nonfiction' AuthorName
    deriving (Eq, Show)

-- The following is also in its normal form.
data Expr =
    Number Int
    | Add Expr Expr
    | Minus Expr
    | Mult Expr Expr
    | Divide Expr Expr