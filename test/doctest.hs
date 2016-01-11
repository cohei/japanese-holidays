module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["src/Data/Time/Calendar/Holiday/Japan.hs"]
