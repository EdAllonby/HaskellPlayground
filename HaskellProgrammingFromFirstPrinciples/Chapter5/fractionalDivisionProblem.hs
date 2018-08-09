-- Before, 6 / length [1, 2, 3] would not work because length returns an Int
-- (/) Requires a Fractional, which Int isn't included in.
-- We can convert an Int into a Num using fromIntegral.

working = 6 / fromIntegral (length [1, 2, 3])