-- a -> a
-- a has the kind *

-- a -> b a -> T (b a)
-- b has the kind * -> * because it takes one argument but needs to return a kind * because it is a part of (->)
-- T has the kind * -> * because it takes one argument (the result of (b a) which has kind *)

-- c a b -> c b a
-- c has kind * -> * -> * because it takes 2 arguments