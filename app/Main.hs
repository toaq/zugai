{-# LANGUAGE OverloadedStrings #-}
module Main where

import Dictionary
import Lex
import Lib
import Parse
import Tree
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    dict <- readDictionary
    let gloss = glossWith dict
    line <- T.getLine
    let Right parsed = parseDiscourse =<< lexToaq line
    T.putStrLn $ treeToLatex (Just gloss) $ toTree parsed
