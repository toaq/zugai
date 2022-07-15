{-# LANGUAGE FlexibleInstances #-}

module English where

-- This module convers zugai parse trees to bad English. %)

import Data.Char
import Data.Foldable
import Data.Text (Text)
import Dictionary
import qualified Data.Text as T
import Lex
import Parse
import Text.Parsec (SourcePos)

class ToEnglish a where
    toEnglish :: Dictionary -> a -> Text

capitalize :: Text -> Text
capitalize t = T.toUpper (T.take 1 t) <> T.drop 1 t

sentencify :: Text -> Text
sentencify t = capitalize t <> (if T.last t `T.elem` ".?!" then "" else ".")

fixUp :: Text -> Text
fixUp "ASS" = "."
fixUp "PST" = "did"
fixUp "PRE" = "now"
fixUp "FUT" = "will"
fixUp "hereby" = "!"
fixUp "optative" = "!"
fixUp "promissive" = "."
fixUp "it.I" = "they"
fixUp "new.topic" = "so,"
fixUp t | "it." `T.isPrefixOf` t = "it"
fixUp t = t

instance ToEnglish Discourse where
    toEnglish d (Discourse xs) = T.unwords (toEnglish d <$> xs)
instance ToEnglish DiscourseItem where
    toEnglish d (DiSentence x) = sentencify $ toEnglish d x
    toEnglish d (DiFragment x) = sentencify $ toEnglish d x
    toEnglish d (DiFree x) = sentencify $ toEnglish d x
instance ToEnglish Sentence where
    toEnglish d (Sentence sc stmt ill) = maybe "" ((<>" ").toEnglish d) sc <> toEnglish d stmt <> maybe "" ((""<>).toEnglish d) ill
instance ToEnglish Fragment where
    toEnglish d (FrPrenex x) = toEnglish d x
    toEnglish d (FrTerms ts) = T.intercalate ", " (toEnglish d <$> toList ts)
instance ToEnglish Prenex where
    toEnglish d (Prenex ts bi) = T.intercalate ", " (toEnglish d <$> toList ts) <> ": "
instance ToEnglish Statement where
    toEnglish d (Statement (Just prenex) preds) = toEnglish d prenex <> toEnglish d preds
    toEnglish d (Statement Nothing preds) = toEnglish d preds
instance ToEnglish PredicationsRubi where
    toEnglish d (Rubi p1 ru bi ps) = toEnglish d p1 <> ", " <> toEnglish d ru <> " " <> toEnglish d ps
    toEnglish d (NonRubi p) = toEnglish d p
instance ToEnglish PredicationC where
    toEnglish d (CompPredication comp stmt) = toEnglish d comp <> " " <> toEnglish d stmt
    toEnglish d (SimplePredication pred) = toEnglish d pred
instance ToEnglish PredicationS where
    toEnglish d (Predication predicate []) = toEnglish d predicate
    toEnglish d (Predication predicate (t:ts)) = T.unwords (toEnglish d t : toEnglish d predicate : (toEnglish d <$> ts))
instance ToEnglish Predicate where
    toEnglish d (Predicate vp) = toEnglish d vp
instance ToEnglish Term where
    toEnglish d (Tnp t) = toEnglish d t
    toEnglish d (Tadvp t) = toEnglish d t
    toEnglish d (Tpp t) = toEnglish d t
    toEnglish d (Termset to (W (Pos _ _ ru) _) t1 to' t2) =
        T.unwords [forethoughtToEnglish ru, (T.intercalate ", " $ toEnglish d <$> t1), toEnglish d ru, (T.intercalate ", " $ toEnglish d <$> t2)]

instance (ToEnglish t) => ToEnglish (Connable' na t) where
    toEnglish d (Conn x na ru y) =
        T.unwords [toEnglish d x, toEnglish d ru, toEnglish d y]
    toEnglish d (ConnTo to (W (Pos _ _ ru) _) x to' y) =
        T.unwords [forethoughtToEnglish ru, toEnglish d x, toEnglish d ru, toEnglish d y]
    toEnglish d (Single x) = toEnglish d x

instance ToEnglish AdvpC where
    toEnglish d (Advp vp) = toEnglish d vp <> "ly"
instance ToEnglish PpC where
    toEnglish d (Pp prep np) = toEnglish d prep <> " " <> toEnglish d np
instance ToEnglish PrepC where
    toEnglish d (Prep vp) = toEnglish d vp
instance ToEnglish NpC where
    toEnglish d (Focused foc np) = (toEnglish d foc) <> " " <> (toEnglish d np)
    toEnglish d (Unf np) = toEnglish d np
instance ToEnglish NpF where
    toEnglish d (ArgRel arg rel) = toEnglish d arg <> " which [" <> toEnglish d rel <> "]"
    toEnglish d (Unr np) = toEnglish d np
instance ToEnglish NpR where
    toEnglish d (Bound vp) = toEnglish d vp
    toEnglish d (Ndp dp) = toEnglish d dp
    toEnglish d (Ncc cc) = toEnglish d cc
instance ToEnglish Dp where
    toEnglish d (Dp det (Just vp)) = toEnglish d det <> " " <> toEnglish d vp
    toEnglish d (Dp det Nothing) = toEnglish d det
instance ToEnglish RelC where
    toEnglish d (Rel pred tmr) = toEnglish d pred
instance ToEnglish Cc where
    toEnglish d (Cc pred tmr) = "[" <> toEnglish d pred <> "]"
instance ToEnglish VpC where
    toEnglish d (Serial x y) = toEnglish d x <> "–" <> toEnglish d y
    toEnglish d (Nonserial x) = toEnglish d x
instance ToEnglish VpN where
    toEnglish d (Vname nv name tmr) = capitalize (toEnglish d name)
    toEnglish d (Vshu shu text) = "\"" <> posSrc text <> "\""
    toEnglish d (Voiv oiv np tmr) = toEnglish d oiv <> " " <> toEnglish d np
    toEnglish d (Vmo mo disc teo) = "«" <> toEnglish d disc <> "»"
    toEnglish d (Vlu lu stmt ky) = "that-which " <> toEnglish d stmt
    toEnglish d (Vverb w) = toEnglish d w
instance ToEnglish Name where
    toEnglish d (VerbName x) = toEnglish d x
    toEnglish d (TermName x) = toEnglish d x
instance ToEnglish FreeMod where
    toEnglish d (Fint teto) = toEnglish d teto
    toEnglish d (Fvoc hu np) = "hey " <> toEnglish d np
    toEnglish d (Finc ju sentence) = "(" <> toEnglish d sentence <> ")"
    toEnglish d (Fpar kio disc ki) = "(" <> toEnglish d disc <> ")"
instance ToEnglish t => ToEnglish (Pos t) where
    toEnglish d (Pos _ _ t) = toEnglish d t
instance ToEnglish t => ToEnglish (W t) where
    toEnglish d (W w fms) = T.unwords (toEnglish d w : (toEnglish d <$> fms))
instance ToEnglish (Text, Tone) where
    toEnglish d (t, _) = toEnglish d t
instance ToEnglish Text where
    toEnglish d t = case fixUp $ glossWith d t of "???" -> t; e -> e
instance ToEnglish Determiner where
    toEnglish _ Sa = "some"
    toEnglish _ Tu = "every"
    toEnglish _ Tuq = "all"
    toEnglish _ Sia = "no"
    toEnglish _ Ke = "the"
    toEnglish _ Hoi = "the"
    toEnglish _ Baq = ""
    toEnglish _ Hi = "which"
    toEnglish _ Ja = "λ"
    toEnglish _ Co = "how many"
    toEnglish _ Kaga = "most"
    toEnglish _ Puy = "many"
    toEnglish d (XShi det) = toEnglish d det <> " single"

instance ToEnglish Connective where
    toEnglish d Ra = "and/or"
    toEnglish d Ri = "or"
    toEnglish d Ru = "and"
    toEnglish d Ro = "or"
    toEnglish d Roi = "and"
    
forethoughtToEnglish :: Connective -> Text
forethoughtToEnglish Ra = "either/both"
forethoughtToEnglish Ri = "which of"
forethoughtToEnglish Ru = "both"
forethoughtToEnglish Ro = "either"
forethoughtToEnglish Roi = "jointly"

instance ToEnglish Complementizer where
    toEnglish d La = "that"
    toEnglish d Ma = "whether"
    toEnglish d Tio = "how much"
