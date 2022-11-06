module Cli where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
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

data Theme = Dark | Light deriving (Eq, Ord, Show)

data CliOptions = CliOptions
  { inputMode :: InputMode,
    outputMode :: OutputMode,
    theme :: Theme,
    lineByLine :: Bool
  }

parseCli :: Parser CliOptions
parseCli =
  CliOptions
    <$> parseInputMode
    <*> parseOutputMode
    <*> flag Dark Light (long "light-theme" <> help "Prefer light-theme output when applicable")
    <*> flag False True (long "line-by-line" <> help "Process each line in the input as a separate text")

cliInfo :: ParserInfo CliOptions
cliInfo = info (parseCli <**> helper) (fullDesc <> progDesc "Parse and interpret Toaq text.")

applyTemplate :: FilePath -> BS.ByteString -> IO BS.ByteString
applyTemplate path string = do
  contents <- BS.readFile path
  let (pre, post) = BS.breakSubstring "%OUTPUT%" contents
  let post' = BS.drop 8 post -- length "%OUTPUT%"
  pure $ pre <> string <> post'

enableLatexLightTheme :: BS.ByteString -> BS.ByteString
enableLatexLightTheme = BSC.unlines . map (\x -> if "\\iffalse" `BS.isPrefixOf` x then "\\iftrue" else x) . BSC.lines

applyTemplateFor :: OutputMode -> Theme -> BS.ByteString -> IO BS.ByteString
applyTemplateFor ToXbarLatex theme text =
  (if theme == Light then enableLatexLightTheme else id) <$>
    applyTemplate "data/templates/xbar.tex" text
applyTemplateFor ToBoxes _ text = applyTemplate "data/templates/boxes.html" text
applyTemplateFor _ _ text = pure text
