module XbarToLatex (xbarToLatex) where

import Data.Char
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
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

-- Convert an Xbar tree to LaTeX \usepackage{forest} format.
xbarToLatex :: Maybe (Text -> Text) -> (Xbar, Movements) -> Text
xbarToLatex annotate (xbar, Movements movements coixs traces) =
  "\\begin{forest}\n[,phantom" <> go xbar <> "[,phantom,tikz={" <> T.unwords (map goMove movements) <> "}]]\\end{forest}"
  where
    cn = coindexationNames coixs
    isMoved i = i `elem` traces
    traceChildren = indicesBelow traces xbar
    tshow = T.pack . show
    node i label children =
      "[" <> label
        <> case cn M.!? i of Just c -> "$_" <> T.cons c "$"; Nothing -> ""
        <> ",tikz={\\node [name=n"
        <> tshow i
        <> ",inner sep=0,fit to=tree]{};}"
        <> children
        <> "]"
    label = T.replace "𝑣" "$v$" . T.replace "◌" "o"
    go (Leaf i src) = node i (goSrc i (label src)) ""
    go (Roof i t src) = node i (label t) ("[" <> goSrc i src <> ",roof]")
    go (Tag i t sub) = node i (label t) (go sub)
    go p@(Pair i t x y) =
      -- if isMoved i then go (Roof i t (aggregateSrc p)) else
      -- this causes problems: goMove outputs node names that didn't get generated, so tikz errors
      node i (label t) (go x <> " " <> go y)
    goSrc i src =
      let srci = prettifyToaq src
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
