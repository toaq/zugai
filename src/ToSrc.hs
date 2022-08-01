module ToSrc where

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

(<~>) :: Text -> Text -> Text
(<~>) = combineWords

class ToSrc a where
    toSrc :: a -> Text
    toName :: a -> Text
    toName = normalizeToaq . toSrc

instance ToSrc Discourse where
    toSrc (Discourse xs) = T.unwords (toSrc <$> xs)
instance ToSrc DiscourseItem where
    toSrc (DiSentence x) = toSrc x
    toSrc (DiFragment x) = toSrc x
    toSrc (DiFree x) = toSrc x
instance ToSrc Sentence where
    toSrc (Sentence sc stmt ill) = T.unwords $ catMaybes [toSrc <$> sc, Just (toSrc stmt), toSrc <$> ill]
instance ToSrc Fragment where
    toSrc (FrPrenex x) = toSrc x
    toSrc (FrTopic t) = toSrc t
instance ToSrc Prenex where
    toSrc (Prenex ts bi) = T.unwords $ (toSrc <$> toList ts) <> [toSrc bi]
instance ToSrc Statement where
    toSrc (Statement mc mp preds) =
        let pp = T.unwords $ catMaybes [toSrc <$> mp, Just $ toSrc preds]
        in case mc of Just c -> toSrc c <~> pp; Nothing -> pp
instance ToSrc PredicationC where
    toSrc (Predication predicate aa nn bb) = T.unwords (toSrc predicate : (toSrc <$> aa) ++ (toSrc <$> nn) ++ (toSrc <$> bb))
instance ToSrc Predicate where
    toSrc (Predicate vp) = toSrc vp
instance ToSrc Adverbial where
    toSrc (Tadvp t) = toSrc t
    toSrc (Tpp t) = toSrc t
instance ToSrc Topic where
    toSrc (Topica t) = toSrc t
    toSrc (Topicn t) = toSrc t

instance (ToSrc t) => ToSrc (Connable' na t) where
    toSrc (Conn x na ru y) =
        T.unwords [toSrc x, toSrc ru, toSrc y]
    toSrc (ConnTo to ru x to' y) =
        T.unwords [toSrc to, toSrc ru, toSrc x, toSrc to', toSrc y]
    toSrc (Single x) = toSrc x

instance ToSrc AdvpC where
    toSrc (Advp t7 vp) = toSrc t7 <~> toSrc vp
instance ToSrc PpC where
    toSrc (Pp prep np) = toSrc prep <> " " <> toSrc np
instance ToSrc PrepC where
    toSrc (Prep t6 vp) = toSrc t6 <~> toSrc vp
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
    toSrc (Dp det (Just vp)) = toSrc det <~> toSrc vp
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
