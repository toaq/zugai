{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Dictionary

main :: IO ()
main = do
    r <- readDictionary
    print $ glossWith r "kijetesaqtakalu"
