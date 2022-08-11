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
showXbarAnsi :: Xbar -> Movements -> [Text]
showXbarAnsi xbar (Movements moves coixs traces) = go xbar
  where
    cn = getCoindexationName coixs
    traceChildren = indicesBelow traces xbar
    mv i t = T.concat markers <> t <> coindex
      where
        mark s n = "\x1b[38;5;34m" <> s <> T.pack (show n) <> " \x1b[0m"
        markers = do
          (n, Movement src tgt) <- zip [1 ..] moves
          [if i == src then mark "←" n else if i == tgt then mark "→" n else ""]
        coindex = maybe "" (\s -> "\x1b[38;5;39m[" <> s <> "]\x1b[0m") (cn i)
    go (Leaf i src) =
      let col = if i `elem` traceChildren then "\x1b[90;9m" else "\x1b[38;5;208m"
       in [col <> prettifyToaq src <> "\x1b[0m"]
    go (Roof i t src) = [mv i t <> "  " <> "\x1b[38;5;208m" <> prettifyToaq src <> "\x1b[0m"]
    go (Tag i t sub) =
      case go sub of
        [one] -> [mv i t <> "  " <> one]
        many -> mv i t : map ("  " <>) many
    go (Pair i t x y) = mv i t : map ("  " <>) (go x) ++ map ("  " <>) (go y)

lpx :: Text -> IO ()
lpx text = do
  dict <- readDictionary
  let Right tokens = lexToaq text
  let Right discourse = parseDiscourse tokens
  let (xbar, movements) = runXbarWithMovements dict discourse
  mapM_ T.putStrLn (showXbarAnsi xbar movements)
