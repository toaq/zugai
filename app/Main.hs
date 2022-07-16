{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import Control.Exception
import Control.Monad
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Aeson.Micro qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Options.Applicative
import Diagrams.Prelude (mkHeight)
import Diagrams.Backend.SVG
import Diagrams.Core.Compile (renderDia)
import Graphics.Svg.Core (renderBS)

import Dictionary
import English
import Interpret
import Lex
import Lib
import Parse hiding (Parser)
import Xbar
import XbarDiagram

data InputMode = FromStdin | FromFile String

parseInputMode :: Parser InputMode
parseInputMode = (FromFile <$> strOption (long "input" <> short 'i' <> metavar "FILENAME" <> help "File to read input from")) <|> pure FromStdin

data OutputMode
    = ToZugaiParseTree
    | ToXbarLatex
    | ToXbarHtml
    | ToXbarJson
    | ToXbarSvg
    | ToEnglish
    | ToLogic deriving Eq
parseOutputMode :: Parser OutputMode
parseOutputMode =
    flag' ToZugaiParseTree (long "to-zugai-tree" <> help "Output mode: dump zugai's internal parse tree")
    <|> flag' ToXbarLatex (long "to-xbar-latex" <> help "Output mode: a LaTeX document of X-bar trees")
    <|> flag' ToXbarHtml (long "to-xbar-html" <> help "Output mode: HTML X-bar tree")
    <|> flag' ToXbarJson (long "to-xbar-json" <> help "Output mode: JSON X-bar tree")
    <|> flag' ToXbarSvg (long "to-xbar-svg" <> help "Output mode: SVG X-bar tree")
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

data ZugaiException = ZugaiException String

instance Exception ZugaiException
instance Show ZugaiException where
    show (ZugaiException s) = s

unwrap :: Show a => Either a b -> IO b
unwrap (Left a) = throwIO (ZugaiException $ show a)
unwrap (Right b) = pure b

processInput :: OutputMode -> Dictionary -> Text -> IO ()
processInput om dict unstrippedInput = do
    let input = T.strip unstrippedInput
    lexed <- unwrap (lexToaq input)
    parsed <- unwrap (parseDiscourse lexed)
    let enc = BSL.fromStrict . encodeUtf8
    let output = case om of
            ToZugaiParseTree -> enc $ T.pack $ show parsed
            ToXbarLatex -> enc $ input <> "\n\n" <> xbarToLatex (Just (glossWith dict)) (toXbar parsed) <> "\n"
            ToXbarHtml -> enc $ xbarToHtml (Just (glossWith dict)) (toXbar parsed)
            ToXbarJson -> BSL.fromStrict $ J.encodeStrict $ xbarToJson (Just (glossWith dict)) (toXbar parsed)
            ToXbarSvg -> renderBS $ renderDia SVG (SVGOptions (mkHeight 500) Nothing "" [] True) (xbarToDiagram (glossWith dict) (toXbar parsed))
            ToEnglish -> enc $ toEnglish dict parsed
            ToLogic -> enc $ T.intercalate "\n" $ map showFormula $ interpret dict parsed
    BSL.putStr (output <> "\n")

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
