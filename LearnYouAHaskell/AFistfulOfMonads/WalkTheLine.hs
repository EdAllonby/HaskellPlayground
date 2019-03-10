module WalkTheLine where

type Birds = Int

type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left + n, right)

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right + n)

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

example :: Pole
example = (0, 0) -: landLeft 1 -: landRight 1 -: landLeft 2

landLeftImproved :: Birds -> Pole -> Maybe Pole
landLeftImproved n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRightImproved :: Birds -> Pole -> Maybe Pole
landRightImproved n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

example2 :: Maybe Pole
example2 = return (0, 0)
  >>= landLeftImproved 1
  >>= landRightImproved 1
  >>= landLeftImproved 2

exampleFail :: Maybe Pole
exampleFail = return (0, 0)
  >>= landLeftImproved 1
  >>= landRightImproved 4
  >>= landLeftImproved (-1)

banana :: Pole -> Maybe Pole
banana _ = Nothing

routine :: Maybe Pole
routine = case landLeftImproved 1 (0, 0) of
  Nothing    -> Nothing
  Just pole1 -> case landRightImproved 4 pole1 of
    Nothing    -> Nothing
    Just pole2 -> case landLeftImproved 2 pole2 of
      Nothing    -> Nothing
      Just pole3 -> landLeftImproved 1 pole3

routineDoNotation :: Maybe Pole
routineDoNotation = do
  let start = (0, 0)
  first <- landLeftImproved 1 start
  second <- landRightImproved 4 first
  third <- landLeftImproved 2 second
  landLeftImproved 1 third

routineDoNotationWithSequence :: Maybe Pole
routineDoNotationWithSequence = do
  let start = (0, 0)
  first <- landLeftImproved 1 start
  second <- landRightImproved 4 first
  Nothing
  third <- landLeftImproved 2 second
  landLeftImproved 1 third