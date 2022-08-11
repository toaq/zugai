module XbarToLatex where

import Data.Char
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Dictionary (glossWith, readDictionary)
import Lex
import Parse
import TextUtils
import Xbar
import XbarUtils

colorWord :: Text -> Text
colorWord t = "{\\color[HTML]{" <> color <> "}" <> t <> "}"
  where
    color =
      if isToneSrc t
        then "ff88cc"
        else case last <$> toToken defaultLexOptions (T.unpack $ normalizeToaq t) of
          Right (Verb _) -> "99eeff"
          _ -> "ffcc88"

escapeLatex :: Text -> Text
escapeLatex t = "{" <> T.concatMap latexSym t <> "}"
  where
    latexSym '𝑣' = "$v$"
    latexSym '∃' = "$\\exists$"
    latexSym '∀' = "$\\forall$"
    latexSym 'λ' = "$\\lambda$"
    latexSym c = T.singleton c

-- Convert an Xbar tree to LaTeX \usepackage{forest} format.
xbarToLatex :: Maybe (Text -> Text) -> (Xbar, Movements) -> Text
xbarToLatex annotate (xbar, Movements movements coixs traces) =
  "\\begin{forest}\n[,phantom" <> go xbar <> "[,phantom,tikz={" <> T.unwords (map goMove movements) <> "}]]\\end{forest}"
  where
    cn = coindexationNames coixs
    isMoved i = i `elem` traces
    traceChildren = indicesBelow traces xbar
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
    label = T.replace "◌" "o"
    go (Leaf i src) = node False i (goSrc i (label src)) ""
    go (Roof i t src) = node False i (label t) ("[" <> goSrc i src <> ",roof]")
    go (Tag i t sub) = node False i (label t) (go sub)
    go p@(Pair i t x y) =
      -- if isMoved i then go (Roof i t (aggregateSrc p)) else
      -- this causes problems: goMove outputs node names that didn't get generated, so tikz errors
      node True i (label t) (go x <> " " <> go y)
    goSrc i src =
      let srci = escapeLatex $ prettifyToaq src
          src'
            | src == "" = "$\\varnothing$"
            | isMoved i || i `elem` traceChildren = "\\sout{" <> srci <> "}"
            | otherwise = colorWord srci
       in "\\textsf{" <> src' <> "}" <> note annotate src
    note (Just f) src
      | noteText <- f src,
        noteText /= "" =
        let (cmd, transform) = if T.all isUpper noteText then ("\\textsc", T.toLower) else ("\\textit", id)
         in "\\\\" <> cmd <> "{\\color[HTML]{dcddde}" <> transform noteText <> "}"
    note _ _ = ""
    goMove (Movement i j) = "\\draw[->] (n" <> tshow i <> ") to[out=south,in=south] (n" <> tshow j <> ");"

latexPreamble :: Text
latexPreamble =
  T.unlines
    [ "\\documentclass[preview,border=30pt]{standalone}",
      "\\usepackage{amssymb}",
      "\\usepackage{ulem}",
      "\\usepackage{xcolor}",
      "\\usepackage[linguistics]{forest}",
      "\\usetikzlibrary{arrows.meta}",
      "\\tikzset{>={Stealth[width=2mm,length=2mm]}}",
      "\\begin{document}",
      "\\pagecolor[HTML]{36393E}",
      "\\color[HTML]{DCDDDE}"
    ]

lpl :: Text -> IO ()
lpl text = do
  dict <- readDictionary
  let Right tokens = lexToaq text
  let Right discourse = parseDiscourse tokens
  let tex = xbarToLatex (Just (glossWith dict)) (runXbarWithMovements dict discourse)
  T.writeFile "output.tex" (latexPreamble <> tex <> "\\end{document}")
