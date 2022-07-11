{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import Control.Exception
import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative

import Dictionary
import English
import Interpret
import Lex
import Lib
import Parse hiding (Parser)
import Tree

data InputMode = FromStdin | FromFile String

parseInputMode :: Parser InputMode
parseInputMode = (FromFile <$> strOption (long "input" <> short 'i' <> metavar "FILENAME" <> help "File to read input from")) <|> pure FromStdin

data OutputMode
    = ToZugaiParseTree
    | ToXbarLatex
    | ToXbarHtml
    | ToEnglish
    | ToLogic deriving Eq
parseOutputMode :: Parser OutputMode
parseOutputMode =
    flag' ToZugaiParseTree (long "to-zugai-tree" <> help "Output mode: dump zugai's internal parse tree")
    <|> flag' ToXbarLatex (long "to-xbar-latex" <> help "Output mode: a LaTeX document of X-bar trees")
    <|> flag' ToXbarHtml (long "to-xbar-html" <> help "Output mode: HTML X-bar tree")
    <|> flag' ToEnglish (long "to-english" <> help "Output mode: badly machine-translated English")
    <|> flag' ToLogic (long "to-logic" <> help "Output mode: predicate logic notation")

data CliOptions = CliOptions
  { inputMode :: InputMode
  , outputMode :: OutputMode
  , lineByLine :: Bool }

parseCli :: Parser CliOptions
parseCli = CliOptions <$> parseInputMode <*> parseOutputMode <*> flag False True (long "line-by-line" <> help "Process each line in the input as a separate text")

cliInfo :: ParserInfo CliOptions
cliInfo = info (parseCli <**> helper) (fullDesc <> progDesc "Parse and interpret Toaq text.")

data ZugaiException = ZugaiException String deriving Show

instance Exception ZugaiException

unwrap :: Show a => Either a b -> IO b
unwrap (Left a) = throwIO (ZugaiException $ show a)
unwrap (Right b) = pure b

processInput :: OutputMode -> Dictionary -> Text -> IO ()
processInput om dict unstrippedInput = do
    let input = T.strip unstrippedInput
    lexed <- unwrap (lexToaq input)
    parsed <- unwrap (parseDiscourse lexed)
    let output =
          case om of
            ToZugaiParseTree -> T.pack $ show parsed
            ToXbarLatex -> input <> "\n\n" <> treeToLatex (Just (glossWith dict)) (toTree parsed) <> "\n"
            ToXbarHtml -> treeToHtml (Just (glossWith dict)) (toTree parsed)
            ToEnglish -> "**" <> input <> "** = " <> toEnglish dict parsed
            ToLogic -> T.intercalate "\n" $ map showFormula $ interpret dict parsed
    T.putStrLn output

main :: IO ()
main = do
    CliOptions im om lineByLine <- execParser cliInfo
    input <- case im of FromStdin -> T.getContents; FromFile s -> T.readFile s
    dict <- readDictionary
    when (om == ToXbarLatex) $ T.putStrLn "\\documentclass[preview,border=30pt]{standalone}\n\\usepackage{amssymb}\n\\usepackage{qtree}\n\\begin{document}"
    if lineByLine
        then mapM_ (processInput om dict) (T.lines input)
        else processInput om dict input
    when (om == ToXbarLatex) $ T.putStrLn "\\end{document}"
