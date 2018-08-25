import Data.List
import Data.Bool

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
               Just (a, b, c) -> Node (unfold f a) b (unfold f c)
               Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild = unfold (\x -> bool (Just (x - 1, x - 1, x - 1)) Nothing (x == 0))
