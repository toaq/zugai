{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Boxes
import Cli
import Control.Exception
import Control.Monad
import Data.Aeson.Micro qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS
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

newtype ZugaiException = ZugaiException String

instance Exception ZugaiException

instance Show ZugaiException where
  show (ZugaiException s) = s

unwrap :: Show a => Either a b -> IO b
unwrap (Left a) = throwIO (ZugaiException $ show a)
unwrap (Right b) = pure b

processInput :: OutputMode -> Dictionary -> Text -> IO BS.ByteString
processInput om dict unstrippedInput = do
  let input = T.strip unstrippedInput
  lexed <- unwrap (lexToaq input)
  parsed <- unwrap (parseDiscourse lexed)
  let enc = BSL.fromStrict . encodeUtf8
  let output = case om of
        ToParseTree -> enc $ T.pack $ show parsed
        ToSrc -> enc $ prettifyToaq $ toSrc parsed
        ToBoxesFlat -> enc $ toBoxes BoxesFlat parsed
        ToBoxesNested -> enc $ toBoxes BoxesNested parsed
        ToStructure -> enc $ prettifyToaq $ toSrcPunctuated parsed
        ToXbarLatex -> enc $ xbarToLatex (Just (glossWith dict)) (runXbarWithMovements dict parsed)
        ToXbarJson -> BSL.fromStrict $ J.encodeStrict $ xbarToJson (Just (glossWith dict)) (runXbar dict parsed)
        ToXbarSvg -> renderBS $ renderDia SVG (SVGOptions (mkHeight 500) Nothing "" [] True) (xbarToDiagram (glossWith dict) (runXbarWithMovements dict parsed))
        ToEnglish -> enc $ toEnglish dict parsed
        ToLogic -> enc $ T.intercalate "\n" $ map showFormula $ interpret dict parsed
  pure $ BSL.toStrict output

main :: IO ()
main = do
  CliOptions im om theme lineByLine <- execParser cliInfo
  input <- case im of FromStdin -> T.getContents; FromFile s -> T.readFile s
  dict <- readDictionary
  output <-
    if lineByLine
      then BS.unlines <$> mapM (processInput om dict) (T.lines input)
      else processInput om dict input
  wrapped <- applyTemplateFor om theme output
  BS.putStr wrapped
