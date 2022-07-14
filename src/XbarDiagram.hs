{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module XbarDiagram where

import Data.Text (Text)
import qualified Data.Text as T
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.SVGFonts

import Xbar

discordBg :: Colour Double
discordBg = sRGB24 0x36 0x39 0x3E

bao :: Colour Double
bao = sRGB24 0xE8 0xE8 0xE8

kuao :: Colour Double
kuao = sRGB24 0x80 0xD0 0xFF

toa :: Colour Double -> Double -> Text -> Diagram B
toa color height t =
    let
        str = if t == "" then "∅" else T.unpack t
        d = stroke (textSVG str height) # fc color # lw none # center
    in
        d <> boundingRect (d # pad 1.5) # lcA transparent

xbarToDiagram :: (Text -> Text) -> Xbar -> Diagram B
xbarToDiagram gloss xbar = go 1 xbar {- # showEnvelope # showOrigin -} # frame 0.25 # bg (sRGB24 0x36 0x39 0x3E) where
    conn i j = connectPerim' (with & arrowHead .~ noHead & shaftStyle %~ (lw 1 <> lc bao)) i j (270@@deg) (90@@deg)
    go :: Integer -> Xbar -> Diagram B
    go i xbar = center $ case xbar of
        Leaf t -> (toa kuao 1 t <> toa bao 0.8 (gloss t) # moveTo (0^&(-0.75))) # named i
        Tag t x -> vsep 0.5 [toa bao 1 t # named i, go (2*i) x] # conn i (2*i)
        Pair t x y -> vsep 1 [toa bao 1 t # named i, center (hsep 0.2 [go (2*i) x, go (2*i+1) y])] # conn i (2*i) # conn i (2*i+1)

