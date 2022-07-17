{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

module Xbar where

-- This module convers zugai parse trees to https://toaq-org.netlify.app/parser/ style *binary* (X-bar) parse trees.

import Data.Char
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty(..))
import Lex
import Parse
import Text.Parsec (SourcePos)
import qualified Data.Aeson.Micro as J
import Data.Aeson.Micro ((.=), object)
import ToName

data Xbar
    = Tag Text Xbar
    | Pair Text Xbar Xbar
    | Leaf Text -- Source word and gloss
    deriving (Eq, Show)

class ToXbar a where
    toXbar :: a -> Xbar

-- Turn n≥1 terms into a parse tree with a "term" or "terms" head.
termsToXbar :: Foldable t => t Term -> Xbar
termsToXbar = foldl1 (Pair "Terms") . map toXbar . toList

-- When rendering an elided VP after "sa", this "∅" VP is rendered as a fallback.
nullVp :: SourcePos -> Vp
nullVp sourcePos = Single (Nonserial (Vverb (W (Pos sourcePos "" "") [])))

-- Pair a construct with its optional terminator.
terminated :: Text -> Xbar -> Terminator -> Xbar
terminated _ t Nothing = t
terminated tag t (Just word) = Pair tag t (Tag "End" $ toXbar word)

covert :: Xbar
covert = Leaf ""

prenexToXbar :: NonEmpty Term -> W () -> Xbar -> Xbar
prenexToXbar (t:|[]) bi c = Pair "TopicP" (toXbar t) (Pair "Topic'" (Tag "Topic" $ toXbar bi) c)
prenexToXbar (t:|(t':ts)) bi c = Pair "TopicP" (toXbar t) (Pair "Topic'" (Tag "Topic" covert) (prenexToXbar (t':|ts) bi c))

instance ToXbar Discourse where
    toXbar (Discourse ds) = foldr1 (Pair "Discourse") (toXbar <$> ds)
instance ToXbar DiscourseItem where
    toXbar (DiSentence x) = toXbar x
    toXbar (DiFragment x) = toXbar x
    toXbar (DiFree x) = toXbar x
instance ToXbar Sentence where
    toXbar (Sentence sc stmt ill) =
        let t = case ill of Just i -> Pair "SAP" (toXbar stmt) (Tag "SA" $ toXbar i)
                            Nothing -> Pair "SAP" (toXbar stmt) (Tag "SA" (Leaf ""))
        in case sc of Just sc -> Pair "SAP'" (Tag "SConn" $ toXbar sc) t
                      Nothing -> t
instance ToXbar Fragment where
    toXbar (FrPrenex (Prenex ts bi)) = prenexToXbar ts bi covert
    toXbar (FrTerms ts) = termsToXbar ts
instance ToXbar Statement where
    toXbar (Statement (Just (Prenex ts bi)) preds) = prenexToXbar ts bi (toXbar preds)
    toXbar (Statement Nothing preds) = toXbar preds
instance ToXbar PredicationsRubi where
    toXbar (Rubi p1 ru bi ps) = Pair "VP" (Pair "VP" (toXbar p1) (Pair "Co" (toXbar ru) (Tag "End" $ toXbar bi))) (toXbar ps)
    toXbar (NonRubi p) = toXbar p
instance ToXbar PredicationC where
    toXbar (CompPredication comp stmt) = Pair "CP" (toXbar comp) (toXbar stmt)
    toXbar (SimplePredication pred) = toXbar pred
instance ToXbar PredicationS where
    toXbar (Predication predicate []) = Tag "VP" (toXbar predicate)
    -- toXbar (Predication predicate terms) = Pair "Pred" (toXbar predicate) (termsToXbar terms)
    toXbar (Predication predicate terms) = Pair "VP" (toXbar predicate) $ foldr1 (Pair "Terms") (toXbar <$> terms)
instance ToXbar Predicate where
    toXbar (Predicate vp) = Tag "Verb" (toXbar vp)
instance ToXbar Term where
    toXbar (Tnp t) = toXbar t
    toXbar (Tadvp t) = toXbar t
    toXbar (Tpp t) = toXbar t
    toXbar (Termset to ru t1 to' t2) =
        Pair "Termset"
            (Pair "Co'" (Tag "Co" $ toXbar to) (toXbar ru))
            (Pair "CoP(Termset)" (termsToXbar t1)
                (Pair "Co'" (Tag "Co" $ toXbar to') (termsToXbar t2)))

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
    toXbarNa :: Xbar -> na -> Xbar

instance ToXbarNa () where toXbarNa t () = t
instance ToXbarNa (W ()) where toXbarNa t na = Pair "CoP" t (Tag "End" $ toXbar na)

instance (ToXbar t, ToXbarNa na, ConnName t) => ToXbar (Connable' na t) where
    toXbar (Conn x na ru y) =
        Pair ("CoP(" <> connName @t <> ")")
            (toXbarNa (toXbar x) na)
            (Pair "Co'" (toXbar ru) (toXbar y))
    toXbar (ConnTo to ru x to' y) =
        Pair ("CoP(" <> connName @t <> ")")
            (Pair "Co'" (Tag "Co" $ toXbar to) (toXbar ru))
            (Pair ("CoP(" <> connName @t <> ")") (toXbar x)
                (Pair "Co'" (Tag "Co" $ toXbar to') (toXbar y)))
    toXbar (Single x) = toXbar x

instance ToXbar AdvpC where
    toXbar (Advp vp) = Tag "AdvP" (toXbar vp)
instance ToXbar PpC where
    toXbar (Pp prep np) = Pair "PP" (toXbar prep) (toXbar np)
instance ToXbar PrepC where
    toXbar (Prep vp) = Tag "P" (toXbar vp)
instance ToXbar NpC where
    toXbar (Focused foc np) = Pair "Foc" (toXbar foc) (toXbar np)
    toXbar (Unf np) = toXbar np
instance ToXbar NpF where
    toXbar (ArgRel arg rel) = Pair "NPrel" (toXbar arg) (toXbar rel)
    toXbar (Unr np) = toXbar np
instance ToXbar NpR where
    toXbar (Bound vp) = Tag "DP" (toXbar vp) -- Pair "DP" (Tag "D" $ Leaf "•\x0301") $ Tag "VP" (Leaf $ toName vp)
    toXbar (Ndp dp) = toXbar dp
    toXbar (Ncc cc) = toXbar cc
instance ToXbar Dp where
    toXbar (Dp det@(W pos _) vp) = Pair "DP" (Tag "D" $ Leaf (posSrc pos)) (toXbar $ maybe (nullVp $ posPos pos) id vp)
instance ToXbar RelC where
    toXbar (Rel pred tmr) = terminated "RelP" (toXbar pred) tmr
instance ToXbar Cc where
    toXbar (Cc pred tmr) = terminated "CP" (Tag "CP" $ toXbar pred) tmr
instance ToXbar VpC where
    toXbar (Serial x y) = Pair "Serial" (toXbar x) (toXbar y)
    toXbar (Nonserial x) = toXbar x
instance ToXbar VpN where
    toXbar (Vname nv name tmr) = terminated "Vname" (Pair "Vname" (toXbar nv) (toXbar name)) tmr
    toXbar (Vshu shu text) = Pair "Vquote" (Tag "Quoter" $ toXbar shu) (Tag "Quoted" $ toXbar text)
    toXbar (Voiv oiv np tmr) = terminated "Vinc" (Pair "Vinc" (Tag "OIV" $ toXbar oiv) (toXbar np)) tmr
    toXbar (Vmo mo disc teo) = terminated "Vquote" (Pair "Vquote" (Tag "Quoter" $ toXbar mo) (toXbar disc)) teo
    toXbar (Vlu lu stmt ky) = terminated "Vfree" (Pair "Vfree" (Tag "Free" $ toXbar lu) (toXbar stmt)) ky
    toXbar (Vverb w) = Tag "V" (toXbar w)
instance ToXbar Name where
    toXbar (VerbName x) = toXbar x
    toXbar (TermName x) = toXbar x
instance ToXbar FreeMod where
    toXbar (Fint teto) = Tag "Interj" (toXbar teto)
    toXbar (Fvoc hu np) = Pair "Voc" (toXbar hu) (toXbar np)
    toXbar (Finc ju sentence) = Pair "Inc" (toXbar ju) (toXbar sentence)
    toXbar (Fpar kio disc ki) = Pair "Par" (toXbar kio) (Pair "Par" (toXbar disc) (toXbar ki))
instance ToXbar () where
    toXbar () = Leaf "()"
--instance ToXbar (Pos Text) where
--    toXbar (Pos _ src txt) = Leaf src txt
instance ToXbar t => ToXbar (Pos t) where
    toXbar (Pos _ src t) =
        case toXbar t of
            Leaf _ -> Leaf src
            Tag t (Leaf _) -> Tag t (Leaf src)
            x -> x
instance ToXbar t => ToXbar (W t) where
    toXbar (W w fms) = foldl (Pair "Free") (toXbar w) (toXbar <$> fms)
instance ToXbar Text where toXbar t = Leaf t
instance ToXbar (Text, Tone) where toXbar (t, _) = Leaf t
instance ToXbar String where toXbar t = toXbar (T.pack t)

instance ToXbar NameVerb where toXbar nv = Tag "NameVerb" $ toXbar $ show nv
instance ToXbar Determiner where toXbar det = Tag "D" $ toXbar $ show det
instance ToXbar Connective where toXbar t = Tag "Co" $ toXbar $ show t
instance ToXbar Complementizer where toXbar t = Tag "C" $ toXbar $ show t

-- Cut one-child nodes out of an Xbar tree.
-- predication(predicate(verb(de))) becomes just verb(de).
simplifyXbar :: Xbar -> Xbar
simplifyXbar t@(Leaf _) = t
simplifyXbar t@(Tag _ (Leaf _)) = t
simplifyXbar (Tag _ sub) = simplifyXbar sub
simplifyXbar (Pair t x y) = Pair t (simplifyXbar x) (simplifyXbar y)

-- Show an Xbar tree using indentation and ANSI colors.
showXbarAnsi :: Xbar -> [Text]
showXbarAnsi (Leaf src) = ["\x1b[94m" <> src <> "\x1b[0m"]
showXbarAnsi (Tag t sub) =
    case showXbarAnsi sub of
        [one] -> [t <> ": " <> one]
        many -> (t<>":") : map ("  "<>) many
showXbarAnsi (Pair t x y) = (t<>":") : map ("  "<>) (showXbarAnsi x) ++ map ("  "<>) (showXbarAnsi y)

-- Convert an Xbar tree to LaTeX \usepackage{qtree} format.
xbarToLatex :: Maybe (Text -> Text) -> Xbar -> Text
xbarToLatex annotate xbar = "\\Xbar " <> go xbar
    where
        go (Leaf src) =
            "{\\textsf{ " <> (if src == "" then "$\\varnothing$" else src) <> "}" <> note annotate src <> "}"
        go (Tag t sub) = "[." <> t <> " " <> go sub <> " ]" <> (if False && t == "CP" then " !{\\qframesubtree}" else "")
        go (Pair t x y) = "[." <> t <> " " <> go x <> " " <> go y <> " ]"
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
        go (Leaf src) = div "leaf" (div "src" src <> note annotate src)
        go (Tag t sub) = div "node" (div "tag" t <> div "children" (go sub))
        go (Pair t x y) = div "node" (div "tag" t <> div "children" (go x <> go y))
        note Nothing _ = ""
        note (Just f) src = div "gloss" (f src)

-- Convert an Xbar tree to JSON.
xbarToJson :: Maybe (Text -> Text) -> Xbar -> J.Value
xbarToJson annotate xbar =
    case xbar of
        Leaf src -> object ["type" .= J.String "leaf", "src" .= J.String src, "gloss" .= case annotate of Just f -> J.String (f src); _ -> J.Null]
        Tag t sub -> object ["type" .= J.String "node", "tag" .= J.String t, "children" .= J.Array [xbarToJson annotate sub]]
        Pair t x y -> object ["type" .= J.String "node", "tag" .= J.String t, "children" .= J.Array (xbarToJson annotate <$> [x,y])]
