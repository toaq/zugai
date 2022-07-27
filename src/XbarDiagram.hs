{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module XbarDiagram where

import Data.Text (Text)
import qualified Data.Text as T
import Diagrams.Prelude
import qualified Diagrams.TwoD.Text as TD
import Diagrams.Backend.SVG
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Debug.Trace

import Lex
import TextUtils
import Xbar

pastel :: Double -> Colour Double
pastel hue = uncurryRGB sRGB24 $ fmap (truncate . (* 255)) $ hsl hue 1.0 0.8

wordColor :: Text -> Colour Double
wordColor "" = bao
wordColor t =
    case toToken defaultLexOptions (T.unpack $ normalizeToaq t) of
        -- Left _ -> bao
        -- Right (_, T2) -> pastel 27
        -- Right (_, T3) -> pastel 55
        -- Right (_, T4) -> pastel 120
        -- Right (_, T5) -> pastel 170
        -- Right (_, T6) -> pastel 220
        -- Right (_, T7) -> pastel 280
        -- Right (_, T8) -> pastel 0
        Right (Verb _, _) -> pastel 200
        _ -> pastel 30

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
    let
        str = if t == "" then "∅" else T.unpack t
        color' = if color == rui && t == "" then discordBg else color
        d = TD.text str # TD.font "Linux Libertine O" # TD.fontSizeL height # fc color' # lw none # centerX
        strut' = strut $ flip V2 height $ height / 2 * (fromIntegral (length str))
    in
        d <> boundingRect (d `atop` strut' # frame 0.2) # lcA transparent

xbarToDiagram :: (Text -> Text) -> (Xbar, [Movement]) -> Diagram B
xbarToDiagram gloss (xbar,movements) =
        go 1 xbar
        {- # showEnvelope # showOrigin -}
        # (\d -> foldr goMove d movements)
        # frame 0.25
        # bg (sRGB24 0x36 0x39 0x3E)
        where
    conn     i j = connectPerim' (with & arrowHead .~ noHead & shaftStyle %~ (lw 1 <> lc bao)) i j (270@@deg) (90@@deg)
    connMove i j = connectPerim' (with & headStyle %~ fc bao & shaftStyle %~ (lw 1 <> lc bao)) i j (270@@deg) (270@@deg)
    gloss' = T.unwords . map gloss . T.words
    named' i = named (-1-i) -- ughhgghfhgjfhg
    goMove :: Movement -> Diagram B -> Diagram B
    goMove (Movement i j) dia = dia # connMove (-1-i) (-1-j)
    go :: Integer -> Xbar -> Diagram B
    go i xbar = center $ opacity (if or [j == Xbar.index xbar | Movement j _ <- movements] then 0.5 else 1.0) $ case xbar of
        Leaf j t -> (toa (wordColor t) 1 t <> toa rui 0.8 (gloss t) # moveTo (0^&(-0.75))) # named i # named' j
        Roof j t src -> vsep 0.1 [toa bao 1 t # named i, triangle 2 # lw 1 # lc bao # scaleY 0.4, toa (pastel 200) 1 src, toa rui 0.8 (gloss' src)] # named' j
        Tag j t x -> vsep 0.5 [toa bao 1 t # named i, go (2*i) x] # named' j # conn i (2*i)
        Pair j t x y -> vsep 1 [toa bao 1 t # named i, center (hsep 0.2 [go (2*i) x, go (2*i+1) y])] # named' j # conn i (2*i) # conn i (2*i+1)

