{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Xbar where

-- This module convers zugai parse trees to https://toaq-org.netlify.app/parser/ style *binary* (X-bar) parse trees.

import Control.Monad.State
import Data.Aeson.Micro ((.=), object)
import Data.Aeson.Micro qualified as J
import Data.Char
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Text qualified as T
import Text.Parsec (SourcePos)

import Lex
import Parse
import Scope
import ToName

data Xbar
    = Tag Int Text Xbar
    | Pair Int Text Xbar Xbar
    | Leaf Int Text -- Source word
    | Roof Int Text Text -- Tag and source text
    deriving (Eq, Show)

index :: Xbar -> Int
index (Tag i _ _) = i
index (Pair i _ _ _) = i
index (Leaf i _) = i
index (Roof i _ _) = i

retag :: Text -> Xbar -> Xbar
retag t (Tag i _ x) = Tag i t x
retag t (Pair i _ x y) = Pair i t x y
retag _ (Leaf i s) = Leaf i s
retag t (Roof i _ s) = Roof i t s

mapSrc :: (Text -> Text) -> Xbar -> Xbar
mapSrc f (Tag i t x) = Tag i t (mapSrc f x)
mapSrc f (Pair i t x y) = Pair i t (mapSrc f x) (mapSrc f y)
mapSrc f (Leaf i s) = Leaf i (f s)
mapSrc f (Roof i t s) = Roof i t (f s)

data Movement = Movement Int Int deriving (Eq, Ord, Show)

data XbarState =
    XbarState
        { xbarNodeCounter :: Int
        , xbarScopes :: [Scope Int]
        , xbarMovements :: [Movement]
        } deriving (Eq, Show)

newtype Mx a = Mx { unMx :: State XbarState a } deriving (Functor, Applicative, Monad, MonadState XbarState)

runXbarWithMovements :: Discourse -> (Xbar, [Movement])
runXbarWithMovements d =
    case runState (unMx (toXbar d)) (XbarState 0 [] []) of
        (x, s) -> (x, xbarMovements s)

runXbar :: Discourse -> Xbar
runXbar = fst . runXbarWithMovements

instance HasScopes Int Mx where
    getScopes = gets xbarScopes
    setScopes ss = modify (\xs -> xs { xbarScopes = ss })

nextNodeNumber :: Mx Int
nextNodeNumber = do
    i <- gets xbarNodeCounter
    modify (\s -> s { xbarNodeCounter = i + 1 })
    pure i

class ToXbar a where
    toXbar :: a -> Mx Xbar

mkTag :: Text -> Xbar -> Mx Xbar
mkTag t x = do i <- nextNodeNumber; pure $ Tag i t x

mkPair :: Text -> Xbar -> Xbar -> Mx Xbar
mkPair t x y = do i <- nextNodeNumber; pure $ Pair i t x y

mkLeaf :: Text -> Mx Xbar
mkLeaf t = do i <- nextNodeNumber; pure $ Leaf i t

mkRoof :: Text -> Text -> Mx Xbar
mkRoof t s = do i <- nextNodeNumber; pure $ Roof i t s

move :: Xbar -> Xbar -> Mx ()
move src tgt =
    let m = Movement (index src) (index tgt)
    in modify (\s -> s { xbarMovements = m : xbarMovements s })

-- Turn n≥1 terms into a parse tree with a "term" or "terms" head.
termsToXbar :: Foldable t => t Term -> Mx Xbar
termsToXbar = foldl1 (\ma mb -> do a <- ma; b <- mb; mkPair "Terms" a b) . map toXbar . toList

-- When rendering an elided VP after "sa", this "∅" VP is rendered as a fallback.
nullVp :: SourcePos -> Vp
nullVp sourcePos = Single (Nonserial (Vverb (W (Pos sourcePos "" "") [])))

-- Pair a construct with its optional terminator.
terminated :: Text -> Xbar -> Terminator -> Mx Xbar
terminated _ t Nothing = pure t
terminated tag t (Just word) = mkPair tag t =<< (mkTag "End" =<< toXbar word)

covert :: Mx Xbar
covert = mkLeaf ""

prenexToXbar :: NonEmpty Term -> W () -> Xbar -> Mx Xbar
prenexToXbar (t:|[]) bi c = do
    t1 <- toXbar bi
    t2 <- mkTag "Topic" t1
    t3 <- toXbar t
    t4 <- mkPair "Topic'" t2 c
    mkPair "TopicP" t3 t4
prenexToXbar (t:|(t':ts)) bi c = do
    t1 <- mkTag "Topic" =<< covert
    t2 <- prenexToXbar (t':|ts) bi c
    t3 <- toXbar t
    t4 <- mkPair "Topic'" t1 t2
    mkPair "TopicP" t3 t4

instance ToXbar Discourse where
    toXbar (Discourse ds) = foldl1 (\ma mb -> do a <- ma; b <- mb; mkPair "Discourse" a b) (toXbar <$> ds)
instance ToXbar DiscourseItem where
    toXbar (DiSentence x) = toXbar x
    toXbar (DiFragment x) = toXbar x
    toXbar (DiFree x) = toXbar x
instance ToXbar Sentence where
    toXbar (Sentence msc stmt ill) = do
        t <- do
            sa <- case ill of Just i -> toXbar i; Nothing -> covert
            x <- toXbar stmt
            y <- mkTag "SA" sa
            mkPair "SAP" x y
        case msc of
            Just sc -> do xSConn <- mkTag "SConn" =<< toXbar sc; mkPair "SAP" xSConn t
            Nothing -> pure t
instance ToXbar Fragment where
    toXbar (FrPrenex (Prenex ts bi)) = prenexToXbar ts bi =<< covert
    toXbar (FrTerms ts) = termsToXbar ts
instance ToXbar Statement where
    toXbar (Statement (Just (Prenex ts bi)) preds) = prenexToXbar ts bi =<< toXbar preds
    toXbar (Statement Nothing preds) = toXbar preds
instance ToXbar PredicationsRubi where
    toXbar (Rubi p1 ru bi ps) = do
        tp1 <- toXbar p1
        tru <- toXbar ru
        tbi <- mkTag "End" =<< toXbar bi
        tco <- mkPair "Co" tru tbi
        tvp <- mkPair "VP" tp1 tco
        tps <- toXbar ps
        mkPair "VP" tvp tps
    toXbar (NonRubi p) = toXbar p
instance ToXbar PredicationC where
    toXbar (CompPredication comp stmt) = do x <- toXbar comp; y <- toXbar stmt; mkPair "CP" x y
    toXbar (SimplePredication pred) = do x <- mkTag "C" =<< covert; y <- toXbar pred; mkPair "CP" x y
instance ToXbar PredicationS where
    toXbar (Predication predicate []) = mkTag "VP" =<< toXbar predicate
    -- toXbar (Predication predicate terms) = Pair "Pred" (toXbar predicate) (termsToXbar terms)
    toXbar (Predication predicate [tS,tO]) = do
        xV <- retag "F+V" <$> toXbar predicate
        xDPS <- toXbar tS
        xVTrace <- mapSrc T.toLower <$> toXbar predicate
        xDPO <- toXbar tO
        xV' <- mkPair "V'" xVTrace xDPO
        xVP <- mkPair "VP" xDPS xV'
        move xVTrace xV
        mkPair "FP" xV xVP
    toXbar (Predication predicate [tA,tS,tO]) = do
        xV <- retag "F+𝑣+V" <$> toXbar predicate
        xDPA <- toXbar tA
        xv <- mkLeaf "𝑣"
        xDPS <- toXbar tS
        xVTrace <- mapSrc T.toLower <$> toXbar predicate
        xDPO <- toXbar tO
        xV' <- mkPair "V'" xVTrace xDPO
        xVP <- mkPair "VP" xDPS xV'
        xv' <- mkPair "𝑣'" xv xVP
        xvP <- mkPair "𝑣P" xDPA xv'
        move xVTrace xv
        move xv xV
        mkPair "FP" xV xvP
    toXbar (Predication predicate terms) = do
        tp <- toXbar predicate
        tt <- foldr1 (\ma mb -> do a<-ma;b<-mb; mkPair "Terms" a b) (toXbar <$> terms)
        mkPair "VP" tp tt
instance ToXbar Predicate where
    toXbar (Predicate vp) = {- mkTag "Verb" =<< -} toXbar vp
instance ToXbar Term where
    toXbar (Tnp t) = toXbar t
    toXbar (Tadvp t) = toXbar t
    toXbar (Tpp t) = toXbar t
    toXbar (Termset to ru t1 to' t2) = do
        tto <- mkTag "Co" =<< toXbar to
        tru <- toXbar ru
        ttoru <- mkPair "Co'" tto tru
        tt1 <- termsToXbar t1
        tto' <- mkTag "Co" =<< toXbar to'
        tt2 <- termsToXbar t2
        ti <- mkPair "Co'" tto' tt2
        tj <- mkPair "CoP(Termset)" tt1 ti
        mkPair "Termset" ttoru tj

-- Typeclass for associating a "connectand name" with a type
-- so that we can generate strings like Co(NP), Co(VP), etc. in the generic Connable' instance.
class ConnName t where connName :: Text
instance ConnName NpC where connName = "NP"
instance ConnName VpC where connName = "V"
instance ConnName PredicationC where connName = "Pred"
instance ConnName PrepC where connName = "P"
instance ConnName RelC where connName = "Rel"
instance ConnName AdvpC where connName = "AdvP"
instance ConnName PpC where connName = "PP"

-- A little helper typeclass to deal with "na" in the Connable' instance below.
-- We just want to handle the types na=() (no "na") and na=(W()) (yes "na") differently in toXbar.
class ToXbarNa na where
    toXbarNa :: Xbar -> na -> Mx Xbar

instance ToXbarNa () where toXbarNa t () = pure t
instance ToXbarNa (W ()) where
    toXbarNa t na = do
        t1 <- toXbar na
        t2 <- mkTag "End" t1
        mkPair "CoP" t t2

instance (ToXbar t, ToXbarNa na, ConnName t) => ToXbar (Connable' na t) where
    toXbar (Conn x na ru y) = do
        tx <- toXbar x
        t1 <- toXbarNa tx na
        tr <- toXbar ru
        ty <- toXbar y
        t2 <- mkPair "Co'" tr ty
        mkPair ("CoP(" <> connName @t <> ")") t1 t2
    toXbar (ConnTo to ru x to' y) = do
        tt <- mkTag "Co" =<< toXbar to
        tr <- toXbar ru
        t1 <- mkPair "Co'" tt tr
        tx <- toXbar x
        tt' <- mkTag "Co" =<< toXbar to'
        ty <- toXbar y
        t3 <- mkPair "Co'" tt' ty
        t2 <- mkPair ("CoP(" <> connName @t <> ")") tx t3
        mkPair ("CoP(" <> connName @t <> ")") t1 t2
    toXbar (Single x) = toXbar x

instance ToXbar AdvpC where
    toXbar (Advp vp) = mkTag "AdvP" =<< toXbar vp
instance ToXbar PpC where
    toXbar (Pp prep np) = do x <- toXbar prep; y<-toXbar np; mkPair "PP" x y
instance ToXbar PrepC where
    toXbar (Prep vp) = mkTag "P" =<< toXbar vp
instance ToXbar NpC where
    toXbar (Focused foc np) = do x<-toXbar foc; y<-toXbar np; mkPair "Foc" x y
    toXbar (Unf np) = toXbar np
instance ToXbar NpF where
    toXbar (ArgRel arg rel) = do x<-toXbar arg; y<-toXbar rel; mkPair "NPrel" x y
    toXbar (Unr np) = toXbar np
instance ToXbar NpR where
    toXbar (Bound vp) = mkRoof "DP" (toSrc vp) -- Pair "DP" (Tag "D" $ Leaf "•\x0301") $ Tag "VP" (Leaf $ toName vp)
    toXbar (Ndp dp) = toXbar dp
    toXbar (Ncc cc) = toXbar cc
instance ToXbar Dp where
    toXbar (Dp det@(W pos _) vp) = do
        td <- mkTag "D" =<< mkLeaf (posSrc pos)
        tv <- toXbar $ maybe (nullVp $ posPos pos) id vp
        mkPair "DP" td tv
instance ToXbar RelC where
    toXbar (Rel pred tmr) = do x <- toXbar pred; terminated "RelP" x tmr
instance ToXbar Cc where
    toXbar (Cc pred tmr) = do x <- toXbar pred; terminated "CP" x tmr
instance ToXbar VpC where
    toXbar (Serial x y) = do xx <- toXbar x; yy<-toXbar y; mkPair "Serial" xx yy
    toXbar (Nonserial x) = toXbar x
instance ToXbar VpN where
    toXbar (Vname nv name tmr) = do
        t1 <- toXbar nv
        t2 <- toXbar name
        t3 <- mkPair "Vname" t1 t2
        terminated "Vname" t3 tmr
    toXbar (Vshu shu text) = do
        t1 <- mkTag "Quoter" =<< toXbar shu
        t2 <- mkTag "Quoted" =<< toXbar text
        mkPair "Vquote" t1 t2
    toXbar (Voiv oiv np tmr) = do
        t1 <- mkTag "OIV" =<< toXbar oiv
        t2 <- toXbar np
        t3 <- mkPair "Vinc" t1 t2
        terminated "Vinc" t3 tmr
    toXbar (Vmo mo disc teo) = do
        t1 <- mkTag "Quoter" =<< toXbar mo
        t2 <- toXbar disc
        t3 <- mkPair "Vquote" t1 t2
        terminated "Vquote" t3 teo
    toXbar (Vlu lu stmt ky) = do
        t1 <- mkTag "Free" =<< toXbar lu
        t2 <- toXbar stmt
        t3 <- mkPair "Vfree" t1 t2
        terminated "Vfree" t3 ky
    toXbar (Vverb w) = mkTag "V" =<< toXbar w
instance ToXbar Name where
    toXbar (VerbName x) = toXbar x
    toXbar (TermName x) = toXbar x
instance ToXbar FreeMod where
    toXbar (Fint teto) = mkTag "Interj" =<< toXbar teto
    toXbar (Fvoc hu np) = do x<-toXbar hu; y<-toXbar np; mkPair "Voc" x y
    toXbar (Finc ju sentence) = do x<-toXbar ju; y<-toXbar sentence; mkPair "Inc" x y
    toXbar (Fpar kio disc ki) = do x<-toXbar kio; y<-toXbar disc; z<-toXbar ki; mkPair "Par" x =<< mkPair "Par" y z
instance ToXbar () where
    toXbar () = mkLeaf "()"
--instance ToXbar (Pos Text) where
--    toXbar (Pos _ src txt) = Leaf src txt
instance ToXbar t => ToXbar (Pos t) where
    toXbar (Pos _ src t) = do
        inner <- toXbar t
        case inner of
            Leaf _ _ -> mkLeaf src
            Tag _ t (Leaf _ _) -> mkTag t =<< mkLeaf src
            x -> pure x
instance ToXbar t => ToXbar (W t) where
    toXbar (W w fms) = foldl (\ma mb -> do a<-ma; b<-mb; mkPair "Free" a b) (toXbar w) (toXbar <$> fms)
instance ToXbar Text where toXbar t = mkLeaf t
instance ToXbar (Text, Tone) where toXbar (t, _) = mkLeaf t
instance ToXbar String where toXbar t = toXbar (T.pack t)

instance ToXbar NameVerb where toXbar nv = mkTag "NameVerb" =<< toXbar (show nv)
instance ToXbar Determiner where toXbar det = mkTag "D" =<< toXbar (show det)
instance ToXbar Connective where toXbar t = mkTag "Co" =<< toXbar (show t)
instance ToXbar Complementizer where toXbar t = mkTag "C" =<< toXbar (show t)

-- Show an Xbar tree using indentation and ANSI colors.
showXbarAnsi :: Xbar -> [Text]
showXbarAnsi (Leaf _ src) = ["\x1b[94m" <> src <> "\x1b[0m"]
showXbarAnsi (Roof _ t src) = ["\x1b[95m" <> t <> ": " <> src <> "\x1b[0m"]
showXbarAnsi (Tag _ t sub) =
    case showXbarAnsi sub of
        [one] -> [t <> ": " <> one]
        many -> (t<>":") : map ("  "<>) many
showXbarAnsi (Pair _ t x y) = (t<>":") : map ("  "<>) (showXbarAnsi x) ++ map ("  "<>) (showXbarAnsi y)

-- Convert an Xbar tree to LaTeX \usepackage{qtree} format.
xbarToLatex :: Maybe (Text -> Text) -> Xbar -> Text
xbarToLatex annotate xbar = "\\Tree " <> go xbar
    where
        goSrc src = "\\textsf{ " <> (if src == "" then "$\\varnothing$" else src) <> "}" <> note annotate src
        go (Leaf _ src) = "{" <> goSrc src <> "}"
        go (Roof _ t src) = "\\qroof{" <> goSrc src <> "}." <> t
        go (Tag _ t sub) = "[." <> t <> " " <> go sub <> " ]"
        go (Pair _ t x y) = "[." <> t <> " " <> go x <> " " <> go y <> " ]"
        note Nothing _ = ""
        note (Just f) src =
            let
                noteText = f src
                (cmd, transform) = if T.all isUpper noteText then ("\\textsc", T.toLower) else ("\\textit", id)
            in "\\\\" <> cmd <> "{" <> transform noteText <> "}"

-- Convert an Xbar tree to HTML.
xbarToHtml :: Maybe (Text -> Text) -> Xbar -> Text
xbarToHtml annotate xbar = div "zugai-tree" (go xbar)
    where
        div className content = "<div class=\"" <> className <> "\">" <> content <> "</div>"
        go (Leaf _ src) = div "leaf" (div "src" src <> note annotate src)
        go (Roof _ t src) = div "roof" (div "tag" t <> div "src" src)
        go (Tag _ t sub) = div "node" (div "tag" t <> div "children" (go sub))
        go (Pair _ t x y) = div "node" (div "tag" t <> div "children" (go x <> go y))
        note Nothing _ = ""
        note (Just f) src = div "gloss" (f src)

-- Convert an Xbar tree to JSON.
xbarToJson :: Maybe (Text -> Text) -> Xbar -> J.Value
xbarToJson annotate xbar =
    case xbar of
        Leaf _ src -> object ["type" .= J.String "leaf", "src" .= J.String src, "gloss" .= case annotate of Just f -> J.String (f src); _ -> J.Null]
        Roof _ t src -> object ["type" .= J.String "roof", "tag" .= J.String t, "src" .= J.String src]
        Tag _ t sub -> object ["type" .= J.String "node", "tag" .= J.String t, "children" .= J.Array [xbarToJson annotate sub]]
        Pair _ t x y -> object ["type" .= J.String "node", "tag" .= J.String t, "children" .= J.Array (xbarToJson annotate <$> [x,y])]
