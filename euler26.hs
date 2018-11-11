#!/usr/bin/env runhaskell

import Data.List (sortOn)
import System.Environment (getArgs)

chain (0:_) _ _ = []
chain dividends dividend divisor
    | length dividends > 0 && (head dividends) `elem` (tail dividends) = dividends
    | dividend < divisor = chain dividends (dividend * 10) divisor
    | otherwise = chain (dv':dividends) dv' divisor
    where
        dv' = dividend - divisor * m
        m = maxMultiple 1 divisor dividend

        maxMultiple multiplicand multiplier limit
            | (multiplicand + 1) * multiplier > limit = multiplicand
            | otherwise = maxMultiple (multiplicand + 1) multiplier limit

main = do
    args <- getArgs
    let n = read . head $ args
    print . head . sortOn ((1 -) . length . (chain [] 1)) $ [1 .. n - 1]
