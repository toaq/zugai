module XbarToLatex where

import Cli
import Data.ByteString qualified as BS
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Debug.Trace (traceShow)
import Dictionary (glossWith, readDictionary)
import Lex
import Parse
import TextUtils
import Xbar
import XbarUtils

colorWord :: Text -> Text
colorWord t = "{\\color{" <> color <> "}" <> t <> "}"
  where
    t' = T.filter (\c -> c /= '{' && c /= '}') t
    color =
      if isToneSrc t' || "[" `T.isPrefixOf` t'
        then "other"
        else case last <$> toToken defaultLexOptions (T.unpack $ normalizeToaq t) of
          Right (Verb _) -> "verb"
          _ -> "particle"

escapeLatex :: Text -> Text
escapeLatex t = "{" <> T.concatMap latexSym t <> "}"
  where
    latexSym '𝑣' = "$v$"
    latexSym '∃' = "$\\exists$"
    latexSym '∀' = "$\\forall$"
    latexSym 'λ' = "$\\lambda$"
    latexSym '℩' = "\\rotatebox[origin=C]{180}{$\\iota$}"
    latexSym '◌' = "o"
    latexSym c = T.singleton c

showLabelLatex :: Label -> Text
showLabelLatex (Label cat fs) = showCat cat <> T.concat (showFeature <$> fs)
  where
    showCat (Head h) = showHead h
    showCat (X_F c) = showCat c <> "\\textsubscript{F}"
    showCat (X' c) = showCat c <> "'"
    showCat (XP c) = showCat c <> "P"
    showCat (Xplus c) = showCat c <> "+"
    showHead Hv = "$v$"
    showHead h = T.tail . T.pack . show $ h
    showFeature PlusLambda = " [+\\lambda]"

-- Convert an Xbar tree to LaTeX \usepackage{forest} format.
xbarToLatex :: Maybe (Text -> Text) -> (Xbar d, Movements) -> Text
xbarToLatex annotate (xbar, Movements movements coixs) =
  "\\begin{forest}\n[,phantom" <> go xbar <> "[,phantom,tikz={" <> T.unwords (map goMove movements) <> "}]]\\end{forest}"
  where
    cn = coindexationNames coixs
    tshow = T.pack . show
    node canBox i label children =
      let drawBox = canBox && or [i == src | Movement src tgt <- movements]
       in "[" <> escapeLatex label
            <> case cn M.!? i of Just c -> "$_" <> T.cons c "$"; Nothing -> ""
            <> (if drawBox then ",fit=band" else "")
            <> ",tikz={\\node [name=n"
            <> tshow i
            <> ",inner sep=0,fit to=tree"
            <> (if drawBox then ",draw=gray,dashed" else "")
            <> "]{};}"
            <> children
            <> "]"
    unring = T.replace "◌" "o"
    go (Leaf i (Overt "")) = "" -- why do we need this again dfjghdkhkj
    go (Leaf i (Covert "")) = ""
    go (Leaf i src) = node False i (goSrc i src) ""
    go (Roof i _d t src) = node False i (goLabel t) ("[" <> goSrc i src <> ",roof]")
    go (Tag i _d t sub) = node False i (goLabel t) (go sub)
    go p@(Pair i _d t x y) =
      -- if isMoved i then go (Roof i t (aggregateSrc p)) else
      -- this causes problems: goMove outputs node names that didn't get generated, so tikz errors
      node True i (goLabel t) (go x <> " " <> go y)
    goSrc i source =
      let src = sourceText source
          src'
            | src == " " = "$\\varnothing$"
            | otherwise = case source of
              Covert src -> colorWord $ "[" <> escapeLatex src <> "]"
              Traced src -> "\\sout{" <> escapeLatex (prettifyToaq src) <> "}"
              Overt src -> colorWord (escapeLatex (prettifyToaq src))
       in "\\textsf{" <> src' <> "}" <> note annotate src
    goLabel = showLabelLatex
    note (Just f) src
      | noteText <- f src,
        noteText /= "" =
        let (cmd, transform) = if T.all isUpper noteText then ("\\textsc", T.toLower) else ("\\textit", id)
         in "\\\\" <> cmd <> "{\\color{fg}" <> transform noteText <> "}"
    note _ _ = ""
    goMove (Movement i j) = "\\draw[->] (n" <> tshow i <> ") to[out=south,in=south] (n" <> tshow j <> ");"

lpl :: Text -> IO ()
lpl text = do
  dict <- readDictionary
  let Right tokens = lexToaq text
  let Right discourse = parseDiscourse tokens
  let tex = xbarToLatex (Just (glossWith dict)) (runXbarWithMovements dict discourse)
  wrapped <- applyTemplateFor ToXbarLatex Dark (encodeUtf8 tex)
  BS.writeFile "output.tex" wrapped
  putStrLn "Wrote to output.tex."
