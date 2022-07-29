module ToName where

import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Lex
import Parse
import TextUtils

-- The interpreter needs to turn parse subtrees back into "names" for variables:
-- When we encounter (Dp Sa (Serial (Verb "de") (Verb "poq"))) so to speak,
-- we want to map that to the name "de poq"
-- so that we can recognize "dé poq" as having the same "name" later.

class ToName a where
    toName :: a -> Text

instance ToName Discourse where toName (Discourse xs) = T.unwords (toName <$> xs)
instance ToName DiscourseItem where
    toName (DiSentence x) = toName x
    toName (DiFragment x) = toName x
    toName (DiFree x) = toName x
instance ToName Sentence where
    toName (Sentence sc stmt ill) = maybe "" ((<>" ").toName) sc <> toName stmt <> maybe "" ((" "<>).toName) ill
instance ToName Fragment where
    toName (FrPrenex x) = toName x
    toName (FrTerms ts) = T.unwords (toName <$> toList ts)
instance ToName Prenex where
    toName (Prenex ts bi) = T.unwords (toName <$> toList ts) <> " bi"
instance ToName Statement where
    toName (Statement mc mp preds) = T.unwords $ catMaybes [toName <$> mc, toName <$> mp, Just $ toName preds]
instance ToName PredicationC where
    toName (Predication predicate ts) = T.unwords (toName predicate : (toName <$> ts))
instance ToName Predicate where
    toName (Predicate vp) = toName vp
instance ToName Term where
    toName (Tnp t) = toName t
    toName (Tadvp t) = toName t
    toName (Tpp t) = toName t
    toName (Termset to (W (Pos _ _ ru) _) t1 to' t2) =
        T.unwords ["to", toName ru, (T.unwords $ toName <$> t1), "to", (T.unwords $ toName <$> t2)]

instance (ToName t) => ToName (Connable' na t) where
    toName (Conn x na ru y) =
        T.unwords [toName x, toName ru, toName y]
    toName (ConnTo to (W (Pos _ _ ru) _) x to' y) =
        T.unwords ["to", toName ru, toName x, "to", toName y]
    toName (Single x) = toName x

instance ToName AdvpC where
    toName (Advp _ vp) = "7" <> toName vp
instance ToName PpC where
    toName (Pp prep np) = toName prep <> " " <> toName np
instance ToName PrepC where
    toName (Prep _ vp) = "6" <> toName vp
instance ToName NpC where
    toName (Focused foc np) = toName foc <> " " <> toName np
    toName (Unf np) = toName np
instance ToName NpF where
    toName (ArgRel arg rel) = toName arg <> " " <> toName rel
    toName (Unr np) = toName np
instance ToName NpR where
    toName (Npro vp) = "2" <> toName vp
    toName (Ndp dp) = toName dp
    toName (Ncc cc) = toName cc
instance ToName Dp where
    toName (Dp det (Just vp)) = toName det <> " " <> toName vp
    toName (Dp det Nothing) = toName det
instance ToName RelC where
    toName (Rel pred tmr) = "3" <> toName pred <> " cy"
instance ToName Cc where
    toName (Cc pred tmr) = "5" <> toName pred <> " cy"
instance ToName VpC where
    toName (Serial x y) = toName x <> " " <> toName y
    toName (Nonserial x) = toName x
instance ToName VpN where
    toName (Vname nv name tmr) = toName nv <> " " <> toName name <> " ga"
    toName (Vshu shu text) = "shu " <> bareToaq (posSrc text)
    toName (Voiv oiv np tmr) = toName oiv <> " " <> toName np <> " ga"
    toName (Vmo mo disc teo) = "mo " <> toName disc <> " teo"
    toName (Vlu lu stmt ky) = "lu " <> toName stmt <> " ky"
    toName (Vverb w) = toName w
instance ToName Name where
    toName (VerbName x) = toName x
    toName (TermName x) = toName x
instance ToName FreeMod where
    toName (Fint teto) = toName teto
    toName (Fvoc hu np) = "hu " <> toName np
    toName (Finc ju sentence) = "ju " <> toName sentence
    toName (Fpar kio disc ki) = "kio " <> toName disc <> " ki"
instance ToName t => ToName (Pos t) where
    toName (Pos _ _ t) = toName t
instance ToName t => ToName (W t) where
    toName (W w fms) = T.unwords (toName w : (toName <$> fms))
instance ToName (Text, Tone) where
    toName (t, _) = toName t
instance ToName Text where
    toName t = t

instance ToName NameVerb where toName = T.toLower . T.pack . show
instance ToName Determiner where toName = T.toLower . T.pack . show
instance ToName Connective where toName = T.toLower . T.pack . show
instance ToName CWord where toName = T.toLower . T.pack . show
instance ToName Complementizer where toName = maybe "" toName . cword

class ToSrc a where
    toSrc :: a -> Text

instance ToSrc Discourse where toSrc (Discourse xs) = T.unwords (toSrc <$> xs)
instance ToSrc DiscourseItem where
    toSrc (DiSentence x) = toSrc x
    toSrc (DiFragment x) = toSrc x
    toSrc (DiFree x) = toSrc x
instance ToSrc Sentence where
    toSrc (Sentence sc stmt ill) = maybe "" ((<>" ").toSrc) sc <> toSrc stmt <> maybe "" ((""<>).toSrc) ill
instance ToSrc Fragment where
    toSrc (FrPrenex x) = toSrc x
    toSrc (FrTerms ts) = T.unwords (toSrc <$> toList ts)
instance ToSrc Prenex where
    toSrc (Prenex ts bi) = T.unwords $ (toSrc <$> toList ts) <> [toSrc bi]
instance ToSrc Statement where
    toSrc (Statement mc mp preds) = T.unwords $ catMaybes [toSrc <$> mc, toSrc <$> mp, Just $ toSrc preds]
instance ToSrc PredicationC where
    toSrc (Predication predicate ts) = T.unwords (toSrc predicate : (toSrc <$> ts))
instance ToSrc Predicate where
    toSrc (Predicate vp) = toSrc vp
instance ToSrc Term where
    toSrc (Tnp t) = toSrc t
    toSrc (Tadvp t) = toSrc t
    toSrc (Tpp t) = toSrc t
    toSrc (Termset to (W (Pos _ _ ru) _) t1 to' t2) =
        T.unwords [toSrc to, toSrc ru, (T.unwords $ toSrc <$> t1), toSrc to', (T.unwords $ toSrc <$> t2)]

instance (ToSrc t) => ToSrc (Connable' na t) where
    toSrc (Conn x na ru y) =
        T.unwords [toSrc x, toSrc ru, toSrc y]
    toSrc (ConnTo to (W (Pos _ _ ru) _) x to' y) =
        T.unwords [toSrc to, toSrc ru, toSrc x, toSrc to', toSrc y]
    toSrc (Single x) = toSrc x

instance ToSrc AdvpC where
    toSrc (Advp _ vp) = toSrc vp
instance ToSrc PpC where
    toSrc (Pp prep np) = toSrc prep <> " " <> toSrc np
instance ToSrc PrepC where
    toSrc (Prep _ vp) = toSrc vp
instance ToSrc NpC where
    toSrc (Focused foc np) = toSrc foc <> " " <> toSrc np
    toSrc (Unf np) = toSrc np
instance ToSrc NpF where
    toSrc (ArgRel arg rel) = toSrc arg <> " " <> toSrc rel
    toSrc (Unr np) = toSrc np
instance ToSrc NpR where
    toSrc (Npro vp) = toSrc vp
    toSrc (Ndp dp) = toSrc dp
    toSrc (Ncc cc) = toSrc cc
instance ToSrc Dp where
    toSrc (Dp det (Just vp)) = toSrc det <> " " <> toSrc vp
    toSrc (Dp det Nothing) = toSrc det
instance ToSrc RelC where
    toSrc (Rel pred tmr) = toSrc pred <> toSrc tmr
instance ToSrc Cc where
    toSrc (Cc pred tmr) = toSrc pred <> toSrc tmr
instance ToSrc VpC where
    toSrc (Serial x y) = toSrc x <> " " <> toSrc y
    toSrc (Nonserial x) = toSrc x
instance ToSrc VpN where
    toSrc (Vname nv name tmr) = toSrc nv <> " " <> toSrc name <> toSrc tmr
    toSrc (Vshu shu text) = toSrc shu <> " " <> toSrc text
    toSrc (Voiv oiv np tmr) = toSrc oiv <> " " <> toSrc np <> " " <> toSrc tmr
    toSrc (Vmo mo disc teo) = T.unwords [toSrc mo, toSrc disc, toSrc teo]
    toSrc (Vlu lu stmt ky) = T.unwords [toSrc lu, toSrc stmt, toSrc ky]
    toSrc (Vverb w) = toSrc w
instance ToSrc Name where
    toSrc (VerbName x) = toSrc x
    toSrc (TermName x) = toSrc x
instance ToSrc FreeMod where
    toSrc (Fint teto) = toSrc teto
    toSrc (Fvoc hu np) = T.unwords [toSrc hu, toSrc np]
    toSrc (Finc ju sentence) = T.unwords [toSrc ju, toSrc sentence]
    toSrc (Fpar kio disc ki) = T.unwords [toSrc kio, toSrc disc, toSrc ki]
instance ToSrc (Pos t) where
    toSrc (Pos _ src _) = src
instance ToSrc (W t) where
    toSrc (W w fms) = T.unwords (toSrc w : (toSrc <$> fms))

instance ToSrc NameVerb where toSrc = T.pack . show
instance ToSrc Determiner where toSrc = T.pack . show
instance ToSrc Connective where toSrc = T.pack . show
instance ToSrc CWord where toSrc = T.pack . show
instance ToSrc Complementizer where toSrc = maybe "" toSrc . cword
instance ToSrc Terminator where
  toSrc = T.pack . maybe "" ((' ' :) . show)
