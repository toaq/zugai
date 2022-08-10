{-# LANGUAGE FlexibleInstances #-}

module English where

-- This module convers zugai parse trees to bad English. %)

import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Dictionary
import Lex
import Parse
import Text.Parsec (SourcePos)
import TextUtils

class ToEnglish a where
  toEnglish :: Dictionary -> a -> Text

sentencify :: Text -> Text
sentencify t =
  case T.unwords (T.words t) of
    "" -> "(Empty sentence)."
    t -> capitalizeFirst t <> (if T.last t `T.elem` ".?!" then "" else ".")

fixUp :: Text -> Text
fixUp "[assertive]" = "."
fixUp "[past]" = "did"
fixUp "[present]" = "now"
fixUp "[future]" = "will"
fixUp "something" = "thing" -- rai
fixUp "[performative]" = "!"
fixUp "[optative]" = "!"
fixUp "[promissive]" = "."
fixUp "it.I" = "they"
fixUp "[new.topic]" = "so,"
fixUp t | "it." `T.isPrefixOf` t = "it"
fixUp t = t

instance ToEnglish Discourse where
  toEnglish d (Discourse xs) = T.unwords (toEnglish d <$> xs)

instance ToEnglish DiscourseItem where
  toEnglish d (DiSentence x) = sentencify $ toEnglish d x
  toEnglish d (DiFragment x) = sentencify $ toEnglish d x
  toEnglish d (DiFree x) = sentencify $ toEnglish d x

instance ToEnglish Sentence where
  toEnglish d (Sentence sc stmt ill) =
    maybe "" ((<> " ") . toEnglish d) sc
      <> toEnglish d stmt
      <> maybe "" (("" <>) . toEnglish d) ill

instance ToEnglish Fragment where
  toEnglish d (FrPrenex x) = toEnglish d x
  toEnglish d (FrTopic t) = toEnglish d t

instance ToEnglish Prenex where
  toEnglish d (Prenex ts bi) =
    T.intercalate ", " (toEnglish d <$> toList ts) <> ": "

instance ToEnglish Statement where
  toEnglish d (Statement mc mp pred) =
    T.unwords $ catMaybes [toEnglish d <$> mc, toEnglish d <$> mp, Just $ toEnglish d pred]

instance ToEnglish PredicationC where
  toEnglish d (Predication predicate aa ns bb) =
    T.unwords (map t aa ++ middle ++ map t bb)
    where
      t x = toEnglish d x
      middle = case ns of
        [] -> [t predicate]
        (s : o) -> t s : t predicate : (t <$> o)

instance ToEnglish Predicate where
  toEnglish d (Predicate vp) = toEnglish d vp

instance ToEnglish Adverbial where
  toEnglish d (Tadvp t) = toEnglish d t
  toEnglish d (Tpp t) = toEnglish d t

instance ToEnglish Topic where
  toEnglish d (Topica t) = toEnglish d t
  toEnglish d (Topicn t) = toEnglish d t

instance (ToEnglish t) => ToEnglish (Connable' na t) where
  toEnglish d (Conn x na ru y) =
    T.unwords [toEnglish d x, toEnglish d ru, toEnglish d y]
  toEnglish d (ConnTo to (W (Pos _ _ ru) _) x to' y) =
    T.unwords [forethoughtToEnglish ru, toEnglish d x, toEnglish d ru, toEnglish d y]
  toEnglish d (Single x) = toEnglish d x

instance ToEnglish AdvpC where
  toEnglish d (Advp _ vp) = toEnglish d vp <> "ly"

instance ToEnglish PpC where
  toEnglish d (Pp prep np) = toEnglish d prep <> " " <> toEnglish d np

instance ToEnglish PrepC where
  toEnglish d (Prep _ vp) = toEnglish d vp

instance ToEnglish NpC where
  toEnglish d (Focused foc np) = toEnglish d foc <> " " <> toEnglish d np
  toEnglish d (Unf np) = toEnglish d np

instance ToEnglish NpF where
  toEnglish d (ArgRel arg rel) = toEnglish d arg <> " which [" <> toEnglish d rel <> "]"
  toEnglish d (Unr np) = toEnglish d np

instance ToEnglish NpR where
  toEnglish d (Npro vp) = toEnglish d vp
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
  toEnglish d (Vname nv name tmr) = capitalizeFirst (T.strip $ toEnglish d name)
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
  toEnglish d t = case fixUp $ glossWith d t of "" -> t; e -> e

instance ToEnglish Determiner where
  toEnglish _ DT2 = "the"
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

instance ToEnglish CWord where
  toEnglish d La = "that"
  toEnglish d Ma = "whether"
  toEnglish d Tio = "how much"

instance ToEnglish Complementizer where
  toEnglish d (CT3 Nothing) = ""
  toEnglish d (CT3 (Just c)) = toEnglish d c
  toEnglish d (CT4 c) = toEnglish d c
  toEnglish d (CT5 Nothing) = ""
  toEnglish d (CT5 (Just c)) = toEnglish d c
