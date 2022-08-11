{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Boxes
import Control.Exception
import Control.Monad
import Data.Aeson.Micro qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Debug.Trace
import Diagrams.Backend.SVG
import Diagrams.Core.Compile (renderDia)
import Diagrams.Prelude (mkHeight)
import Dictionary
import English
import Graphics.Svg.Core (renderBS)
import Interpret
import Lex
import Lib
import Options.Applicative
import Parse hiding (Parser)
import TextUtils
import ToSrc
import Xbar
import XbarToAnsi
import XbarToLatex
import XbarToSvg

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

newtype ZugaiException = ZugaiException String

instance Exception ZugaiException

instance Show ZugaiException where
  show (ZugaiException s) = s

unwrap :: Show a => Either a b -> IO b
unwrap (Left a) = throwIO (ZugaiException $ show a)
unwrap (Right b) = pure b

processInput :: OutputMode -> Dictionary -> Text -> IO BSL.ByteString
processInput om dict unstrippedInput = do
  let input = T.strip unstrippedInput
  lexed <- unwrap (lexToaq input)
  parsed <- unwrap (parseDiscourse lexed)
  let enc = BSL.fromStrict . encodeUtf8
  let output = case om of
        ToParseTree -> enc $ T.pack $ show parsed
        ToSrc -> enc $ prettifyToaq $ toSrc parsed
        ToBoxes -> enc $ toBoxes parsed
        ToStructure -> enc $ prettifyToaq $ toSrcPunctuated parsed
        ToXbarLatex -> enc $ xbarToLatex (Just (glossWith dict)) (runXbarWithMovements dict parsed)
        ToXbarJson -> BSL.fromStrict $ J.encodeStrict $ xbarToJson (Just (glossWith dict)) (runXbar dict parsed)
        ToXbarSvg -> renderBS $ renderDia SVG (SVGOptions (mkHeight 500) Nothing "" [] True) (xbarToDiagram (glossWith dict) (runXbarWithMovements dict parsed))
        ToEnglish -> enc $ toEnglish dict parsed
        ToLogic -> enc $ T.intercalate "\n" $ map showFormula $ interpret dict parsed
  pure output

applyTemplate :: FilePath -> BSL.ByteString -> IO BS.ByteString
applyTemplate path string = do
  contents <- BS.readFile path
  let (pre, post) = BS.breakSubstring "%OUTPUT%" contents
  pure $ pre <> BSL.toStrict string <> post

applyTemplateFor :: OutputMode -> BSL.ByteString -> IO BS.ByteString
applyTemplateFor ToXbarLatex = applyTemplate "data/templates/xbar.tex"
applyTemplateFor ToBoxes = applyTemplate "data/templates/boxes.html"
applyTemplateFor _ = pure . BSL.toStrict

main :: IO ()
main = do
  CliOptions im om lineByLine <- execParser cliInfo
  input <- case im of FromStdin -> T.getContents; FromFile s -> T.readFile s
  dict <- readDictionary
  output <-
    if lineByLine
      then BSL.unlines <$> mapM (processInput om dict) (T.lines input)
      else processInput om dict input
  wrapped <- applyTemplateFor om output
  BS.putStr wrapped
