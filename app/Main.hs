{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import Control.Exception
import Control.Monad
import System.Exit
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Aeson.Micro qualified as J
import Data.ByteString qualified as BS
import Data.String qualified as S
import Options.Applicative
import Diagrams.Prelude (mkHeight)
import Diagrams.Backend.SVG (renderSVG)

import Dictionary
import English
import Interpret
import Lex
import Lib
import Parse hiding (Parser)
import Xbar
import XbarDiagram

data InputSource = FromStdin | FromFile String
data OutputDestination = ToStdout | ToFile String

parseInputSource :: Parser InputSource
parseInputSource = (FromFile <$> strOption (long "input" <> short 'i' <> metavar "FILENAME" <> help "File to read input from")) <|> pure FromStdin

parseOutputDestination :: Parser OutputDestination
parseOutputDestination = (ToFile <$> strOption (long "output" <> short 'o' <> metavar "FILENAME" <> help "File to write output to")) <|> pure ToStdout

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
  { outputMode :: OutputMode
  , inputSource :: InputSource
  , outputDestination :: OutputDestination
  , lineByLine :: Bool }

parseCli :: Parser CliOptions
parseCli = CliOptions <$> parseOutputMode <*> parseInputSource <*> parseOutputDestination <*> flag False True (long "line-by-line" <> help "Process each line in the input as a separate text")

cliInfo :: ParserInfo CliOptions
cliInfo = info (parseCli <**> helper) (fullDesc <> progDesc "Parse and interpret Toaq text.")

data ZugaiException = ZugaiException String

instance Exception ZugaiException
instance Show ZugaiException where
    show (ZugaiException s) = s

unwrap :: Show a => Either a b -> IO b
unwrap (Left a) = throwIO (ZugaiException $ show a)
unwrap (Right b) = pure b

latexTemplate :: BS.ByteString -> BS.ByteString
latexTemplate s = "\\documentclass[preview,border=30pt]{standalone}\n\\usepackage{amssymb}\n\\usepackage{qtree}\n\\begin{document}" <> s <> "\\end{document}"

processInput :: OutputMode -> OutputDestination -> Dictionary -> Text -> IO ()
processInput om od dict unstrippedInput = do
    let input = T.strip unstrippedInput
    lexed <- unwrap (lexToaq input)
    parsed <- unwrap (parseDiscourse lexed)
    output <-
        case om of
            ToXbarSvg -> case od of
                ToStdout -> do BS.putStr "Can't render SVG to standard output (yet)"; exitWith (ExitFailure 1); return Nothing
                ToFile s -> do renderSVG s (mkHeight 500) (xbarToDiagram (glossWith dict) (toXbar parsed)); return Nothing
            _ -> pure $ Just $ case om of
                ToZugaiParseTree -> encodeUtf8 $ T.pack $ show parsed
                ToXbarLatex -> latexTemplate $ encodeUtf8 $ input <> "\n\n" <> xbarToLatex (Just (glossWith dict)) (toXbar parsed) <> "\n"
                ToXbarHtml -> encodeUtf8 $ xbarToHtml (Just (glossWith dict)) (toXbar parsed)
                ToXbarJson -> J.encodeStrict $ xbarToJson (Just (glossWith dict)) (toXbar parsed)
                ToEnglish -> encodeUtf8 $ toEnglish dict parsed
                ToLogic -> encodeUtf8 $ T.intercalate "\n" $ map showFormula $ interpret dict parsed
    case output of
        Just out -> (case od of
            ToStdout -> BS.putStr
            ToFile s -> BS.appendFile s)
            (out <> "\n")
        Nothing -> pure ()

main :: IO ()
main = do
    CliOptions om is od lineByLine <- execParser cliInfo
    input <- case is of FromStdin -> T.getContents; FromFile s -> T.readFile s
    dict <- readDictionary
    case od of ToFile s -> BS.writeFile s BS.empty; _ -> pure ()
    if lineByLine
        then mapM_ (processInput om od dict) (T.lines input)
        else processInput om od dict input
    case od of ToFile s -> BS.putStr ("Wrote to " <> (S.fromString s) <> "\n"); _ -> pure ()
