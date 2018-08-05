module Scope where

-- Scope exercises

-- This won't compile. 'd' is not in scope for function r.
-- area d = pi * (r * r)
-- r = d / 2

-- The following will compile, everything is in scope
area d = pi * (r * r)
    where r = d / 2