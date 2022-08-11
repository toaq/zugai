module Cli where

import Data.ByteString qualified as BS
import Options.Applicative

data InputMode = FromStdin | FromFile String

parseInputMode :: Parser InputMode
parseInputMode = (FromFile <$> strOption (long "input" <> short 'i' <> metavar "FILENAME" <> help "File to read input from")) <|> pure FromStdin

data OutputMode
  = ToParseTree
  | ToSrc
  | ToStructure
  | ToBoxes
  | ToXbarLatex
  | ToXbarJson
  | ToXbarSvg
  | ToEnglish
  | ToLogic
  deriving (Eq)

parseOutputMode :: Parser OutputMode
parseOutputMode =
  flag' ToParseTree (long "to-parse-tree" <> help "Output mode: dump zugai's internal parse tree")
    <|> flag' ToSrc (long "to-src" <> help "Output mode: debug zugai's toSrc")
    <|> flag' ToStructure (long "to-structure" <> help "Output mode: indicate a sentence's structure with punctuation")
    <|> flag' ToBoxes (long "to-boxes" <> help "Output mode: refgram-style HTML boxes")
    <|> flag' ToXbarLatex (long "to-xbar-latex" <> help "Output mode: a LaTeX document of X-bar trees")
    <|> flag' ToXbarJson (long "to-xbar-json" <> help "Output mode: JSON X-bar tree")
    <|> flag' ToXbarSvg (long "to-xbar-svg" <> help "Output mode: SVG X-bar tree")
    <|> flag' ToEnglish (long "to-english" <> help "Output mode: badly machine-translated English")
    <|> flag' ToLogic (long "to-logic" <> help "Output mode: predicate logic notation")

data CliOptions = CliOptions
  { inputMode :: InputMode,
    outputMode :: OutputMode,
    lineByLine :: Bool
  }

parseCli :: Parser CliOptions
parseCli = CliOptions <$> parseInputMode <*> parseOutputMode <*> flag False True (long "line-by-line" <> help "Process each line in the input as a separate text")

cliInfo :: ParserInfo CliOptions
cliInfo = info (parseCli <**> helper) (fullDesc <> progDesc "Parse and interpret Toaq text.")

applyTemplate :: FilePath -> BS.ByteString -> IO BS.ByteString
applyTemplate path string = do
  contents <- BS.readFile path
  let (pre, post) = BS.breakSubstring "%OUTPUT%" contents
  pure $ pre <> string <> post

applyTemplateFor :: OutputMode -> BS.ByteString -> IO BS.ByteString
applyTemplateFor ToXbarLatex = applyTemplate "data/templates/xbar.tex"
applyTemplateFor ToBoxes = applyTemplate "data/templates/boxes.html"
applyTemplateFor _ = pure
