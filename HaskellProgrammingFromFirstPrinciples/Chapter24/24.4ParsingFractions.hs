{-# LANGUAGE OverloadedStrings #-}

module ImportingFractions where

import           Control.Applicative
import           Data.Ratio                     ( (%) )
import           Text.Trifecta

badFraction :: String
badFraction = "1/0"

alsoBad :: String
alsoBad = "10"

shouldWork :: String
shouldWork = "1/2"

shouldAlsoWork :: String
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator   <- decimal
    _           <- char '/'
    denominator <- decimal
    return (numerator % denominator)

main :: IO ()
main = do
    let parseFraction' = parseString parseFraction mempty
    print $ parseFraction' shouldWork
    print $ parseFraction' shouldAlsoWork
    print $ parseFraction' alsoBad
    print $ parseFraction' badFraction -- Because the exception case is at the end, we don't know that it halts our program

thisWillQuitSoon :: IO ()
thisWillQuitSoon = do
    let parseFraction' = parseString parseFraction mempty
    print $ parseFraction' badFraction -- Now our exception case is at the start. The remaining do-statements will not run.
    print $ parseFraction' shouldWork
    print $ parseFraction' shouldAlsoWork
    print $ parseFraction' alsoBad

virtuousFraction :: Parser Rational
virtuousFraction = do
    numerator   <- decimal
    _           <- char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

testVirtuous :: IO ()
testVirtuous = do
    let virtuousFraction' = parseString virtuousFraction mempty
    print $ virtuousFraction' badFraction -- The fail monad means it will not halt the program.
    print $ virtuousFraction' alsoBad
    print $ virtuousFraction' shouldWork
    print $ virtuousFraction' shouldAlsoWork
