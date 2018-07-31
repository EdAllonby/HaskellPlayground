functorJustExample = fmap (++ " Added string.") (Just "start of sentence.")
functorNothingExample = fmap (*2) Nothing