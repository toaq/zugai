module XbarToAnsi where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Dictionary (readDictionary)
import Lex (lexToaq)
import Parse (parseDiscourse)
import TextUtils (prettifyToaq)
import Xbar (runXbarWithMovements)
import XbarUtils

-- Show an Xbar tree using indentation and ANSI colors.
showXbarAnsi :: Xbar d -> Movements -> [Text]
showXbarAnsi xbar (Movements moves coixs) = go xbar
  where
    orange = "\x1b[38;5;208m"
    green = "\x1b[38;5;34m"
    blue = "\x1b[38;5;39m"
    strikeout = "\x1b[90;9m"
    cn = getCoindexationName coixs
    mv i t = T.concat markers <> showLabel t <> coindex
      where
        mark s n = green <> s <> T.pack (show n) <> " \x1b[0m"
        markers = do
          (n, Movement src tgt) <- zip [1 ..] moves
          [if i == src then mark "←" n else if i == tgt then mark "→" n else ""]
        coindex = maybe "" (\s -> blue <> "[" <> s <> "]\x1b[0m") (cn i)
    go (Leaf i src) = [renderSource src <> "\x1b[0m"]
    go (Roof i _d t src) = [mv i t <> "  " <> orange <> renderSource src <> "\x1b[0m"]
    go (Tag i _d t sub) =
      case go sub of
        [one] -> [mv i t <> "  " <> one]
        many -> mv i t : map ("  " <>) many
    go (Pair i _d t x y) = mv i t : map ("  " <>) (go x) ++ map ("  " <>) (go y)
    renderSource (Overt t) = orange <> prettifyToaq t
    renderSource (Covert t) = orange <> t
    renderSource (Traced t) = strikeout <> prettifyToaq t

lpx :: Text -> IO ()
lpx text = do
  dict <- readDictionary
  let Right tokens = lexToaq text
  let Right discourse = parseDiscourse tokens
  let (xbar, movements) = runXbarWithMovements dict discourse
  T.putStrLn $ T.unlines (showXbarAnsi xbar movements)
