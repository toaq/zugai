{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module XbarToSvg where

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.Text (Text)
import Data.Text qualified as T
import Debug.Trace
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude
import Diagrams.TwoD.Text qualified as TD
import Lex
import TextUtils
import Xbar
import XbarUtils

pastel :: Double -> Colour Double
pastel hue = uncurryRGB sRGB24 $ truncate . (* 255) <$> hsl hue 1.0 0.8

wordColor :: Source -> Colour Double
wordColor (Overt "") = bao
wordColor (Covert "") = bao
wordColor (Overt t) =
  case last <$> toToken defaultLexOptions (T.unpack $ normalizeToaq t) of
    Right (Verb _) -> pastel 200
    _ -> pastel 30
wordColor (Covert t) = pastel 30

discordBg :: Colour Double
discordBg = sRGB24 0x36 0x39 0x3E

bao :: Colour Double
bao = sRGB24 0xE8 0xE8 0xE8

rui :: Colour Double
rui = sRGB24 0xD8 0xD8 0xD8

kuao :: Colour Double
kuao = sRGB24 0x80 0xD0 0xFF

toa :: Colour Double -> Double -> Text -> Diagram B
toa color height t =
  let str = if t == "" then "∅" else T.unpack t
      color' = if color == rui && t == "" then discordBg else color
      d = TD.text str # TD.font "Linux Libertine O" # TD.fontSizeL height # fc color' # lw none # centerX
      strut' = strut $ flip V2 height $ 0.6 * height * fromIntegral (length str)
   in d <> boundingRect (d `atop` strut' # frame 0.2) # lcA transparent

labelText :: Label -> Text
labelText = showLabel

xbarToDiagram :: (Text -> Text) -> (Xbar d, Movements) -> Diagram B
xbarToDiagram gloss (xbar, Movements movements coixs) =
  go 1 xbar
    # (\d -> foldr goMove d movements)
    # frame 0.25
    # bg discordBg
  where
    conn i j = connectPerim' (with & arrowHead .~ noHead & shaftStyle %~ (lw 1 <> lc bao)) i j (270 @@ deg) (90 @@ deg)
    moveShaft = arc xDir (-1 / 3 @@ turn)
    connMove i j = connectPerim' (with & headStyle %~ fc bao & arrowShaft .~ moveShaft & shaftStyle %~ (lw 1 <> lc bao)) i j (270 @@ deg) (270 @@ deg)
    gloss' "v" = ""
    gloss' x = T.unwords $ map gloss (T.words x)
    named' i = named (-1 - i) -- ughhgghfhgjfhg
    goMove :: Movement -> Diagram B -> Diagram B
    goMove (Movement i j) dia = dia # connMove (-1 - i) (-1 - j)
    go :: Integer -> Xbar d -> Diagram B
    go i xbar = center $
      case xbar of
        Leaf j t -> (toa (wordColor t) 1 (sourceText t) === toa rui 0.8 (gloss' $ sourceText t)) # named i # named' j
        Roof j _d t src -> vsep 0.1 [toa bao 1 (labelText t) # named i, triangle 2 # lw 1 # lc bao # scaleY 0.4, toa (pastel 200) 1 (sourceText src)] === toa rui 0.8 (gloss' $ sourceText src) # named' j
        Tag j _d t x -> vsep 0.5 [toa bao 1 (labelText t) # named i, go (2 * i) x] # named' j # conn i (2 * i)
        Pair j _d t x y -> vsep 1 [toa bao 1 (labelText t) # named i, center (hsep 0.2 [go (2 * i) x, go (2 * i + 1) y])] # named' j # conn i (2 * i) # conn i (2 * i + 1)
