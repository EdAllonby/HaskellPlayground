class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

-- id just returns the parameter
instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

yesNoIntExample = yesno $ length []
yesNoListExample = yesno "haha"
yesNoMaybeExample = yesno (Just 1)

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal
        then yesResult
        else noResult

yesnoIfExample = yesnoIf Nothing "yes" "no"
yesnoIfExample2 = yesnoIf (Just 2) "yes" "no"