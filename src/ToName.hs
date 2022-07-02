module ToName where

import Data.Foldable
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

instance ToName Discourse where
    toName (Discourse xs) = T.unwords (toName <$> xs)
instance ToName DiscourseItem where
    toName (DiSentence x) = toName x
    toName (DiFragment x) = toName x
    toName (DiFree x) = toName x
instance ToName Sentence where
    toName (Sentence sc stmt ill) = maybe "" ((<>" ").toName) sc <> toName stmt <> maybe "" ((""<>).toName) ill
instance ToName Fragment where
    toName (FrPrenex x) = toName x
    toName (FrTerms ts) = T.unwords (toName <$> toList ts)
instance ToName Prenex where
    toName (Prenex ts bi) = T.unwords (toName <$> toList ts) <> " bi"
instance ToName Statement where
    toName (Statement (Just prenex) preds) = toName prenex <> toName preds
    toName (Statement Nothing preds) = toName preds
instance ToName PredicationsRubi where
    toName (Rubi p1 ru bi ps) = toName p1 <> " ru bi " <> toName ps
    toName (NonRubi p) = toName p
instance ToName PredicationC where
    toName (CompPredication comp stmt) = toName comp <> " " <> toName stmt
    toName (SimplePredication pred) = toName pred
instance ToName PredicationS where
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
    toName (Advp vp) = "7" <> toName vp
instance ToName PpC where
    toName (Pp prep np) = toName prep <> " " <> toName np
instance ToName PrepC where
    toName (Prep vp) = "6" <> toName vp
instance ToName NpC where
    toName (Focused foc np) = toName foc <> " " <> toName np
    toName (Unf np) = toName np
instance ToName NpF where
    toName (ArgRel arg rel) = toName arg <> " " <> toName rel
    toName (Unr np) = toName np
instance ToName NpR where
    toName (Bound vp) = "2" <> toName vp
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
instance ToName Complementizer where toName = T.toLower . T.pack . show
