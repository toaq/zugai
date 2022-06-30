{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Dictionary
import English
import Lex
import Lib
import Parse
import Tree

makeDocument :: Text -> Text
makeDocument t =
    T.unlines
        [ "\\documentclass[preview,border=30pt]{standalone}"
        , "\\usepackage{amssymb}"
        , "\\usepackage{qtree}"
        , "\\begin{document}"
        , t
        , "\\end{document}" ]

main :: IO ()
main = do
    dict <- readDictionary
    let gloss = glossWith dict
    line <- T.getLine
    let Right parsed = parseDiscourse =<< lexToaq line
    -- T.putStrLn $ makeDocument $ treeToLatex (Just gloss) $ toTree parsed
    T.putStrLn (toEnglish dict parsed)
