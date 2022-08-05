{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Debug.Trace
import Options.Applicative
import Diagrams.Prelude (mkHeight)
import Diagrams.Backend.SVG
import Diagrams.Core.Compile (renderDia)
import Graphics.Svg.Core (renderBS)
import Text.RawString.QQ

import Boxes
import Dictionary
import English
import Interpret
import Lex
import Lib
import Parse hiding (Parser)
import ToSrc
import TextUtils
import Xbar
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
    | ToLogic deriving Eq

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
            ToParseTree -> enc $ T.pack $ show parsed
            ToSrc -> enc $ prettifyToaq $ toSrc parsed
            ToBoxes -> enc $ toBoxes parsed
            ToStructure -> enc $ prettifyToaq $ toSrcPunctuated parsed
            ToXbarLatex -> enc $ xbarToLatex (Just (glossWith dict)) (runXbarWithMovements dict parsed)
            ToXbarJson -> BSL.fromStrict $ J.encodeStrict $ xbarToJson (Just (glossWith dict)) (runXbar dict parsed)
            ToXbarSvg -> renderBS $ renderDia SVG (SVGOptions (mkHeight 500) Nothing "" [] True) (xbarToDiagram (glossWith dict) (runXbarWithMovements dict parsed))
            ToEnglish -> enc $ toEnglish dict parsed
            ToLogic -> enc $ T.intercalate "\n" $ map showFormula $ interpret dict parsed
    BSL.putStr (output <> "\n")

latexPreamble :: Text
latexPreamble =
    T.unlines
        [ "\\documentclass[preview,border=30pt]{standalone}"
        , "\\usepackage{amssymb}"
        , "\\usepackage{ulem}"
        , "\\usepackage{xcolor}"
        , "\\usepackage[linguistics]{forest}"
        , "\\usetikzlibrary{arrows.meta}"
        , "\\tikzset{>={Stealth[width=2mm,length=2mm]}}"
        , "\\begin{document}"
        , "\\pagecolor[HTML]{36393E}"
        , "\\color[HTML]{DCDDDE}"
        ]

boxesPreamble :: Text
boxesPreamble = [r|
<html>
<head>
<title>zugai boxes output</title>
<style>
.box {
    font-size: 18px;
    font-family: sans-serif;
    display: flex;
    border-width: 2px;
    margin: 20px 5px 5px 5px;
    align-items: flex-end;
    border-style: solid;
    min-width: 20px;
}
.discourse { border-style: none; flex-direction: column; align-items: flex-start; }
.box.leaf { border-style: none; }
.box { position: relative; }
.box:before { width: 0px; height: 0px; position: absolute; top: 5px; left: 5px; opacity: 0.5; font-size: 12px; }
.sentence          { border-color: #72c91b; background-color: #dafa94; color: black; } .sentence:before { content: "sentence"; }
.clause            { border-color: #b85450; background-color: #f8cecc; color: black; } .clause:before { content: "clause"; }
.complementizer    { border-color: #b20000; background-color: #e51400; color: white; } .complementizer:before { content: "C"; }
.topic             { border-color: #a50040; background-color: #d80073; color: white; } .topic:before { content: "topic"; }
.verbal-complex    { border-color: #2d7600; background-color: #72c91b; color: black; } .verbal-complex:before { content: "verb"; }
.post-field        { border-color: #d79b00; background-color: #ffc41f; color: black; } .post-field:before { content: ""; }
.adverbial         { border-color: #432d57; background-color: #76608a; color: white; } .adverbial:before { content: "adv"; }
.argument          { border-color: #c73500; background-color: #fa6800; color: black; } .argument:before { content: "noun"; }
.sentence-boundary { border-color: #666666; background-color: #eeeeee; color: black; } .sentence-boundary:before { content: "start"; }
.illocution        { border-color: #6c8ebf; background-color: #8bafe4; color: black; } .illocution:before { content: "illoc"; }
</style>
</head>
<body>
|]

main :: IO ()
main = do
    CliOptions im om lineByLine <- execParser cliInfo
    input <- case im of FromStdin -> T.getContents; FromFile s -> T.readFile s
    dict <- readDictionary
    when (om == ToXbarLatex) $ T.putStr latexPreamble
    when (om == ToBoxes) $ T.putStr boxesPreamble
    if lineByLine
        then mapM_ (processInput om dict) (T.lines input)
        else processInput om dict input
    when (om == ToXbarLatex) $ T.putStrLn "\\end{document}"
