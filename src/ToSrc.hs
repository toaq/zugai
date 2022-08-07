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

data ToSrcOptions =
    ToSrcOptions
        { punctuate :: Bool
        } deriving (Eq, Show)

class ToSrc a where
    toSrcWith :: ToSrcOptions -> a -> Text

    toSrc :: a -> Text
    toSrc = toSrcWith (ToSrcOptions { punctuate = False })

    toSrcPunctuated :: a -> Text
    toSrcPunctuated = toSrcWith (ToSrcOptions { punctuate = True })

    toName :: a -> Text
    toName = T.filter (not . isCombiningDiacritic) . normalizeToaq . toSrc

bracket :: ToSrcOptions -> Text -> Text
bracket o t = if punctuate o then "[" <> t <> "]" else t

bold :: ToSrcOptions -> Text -> Text
bold o t = if punctuate o then "**" <> t <> "**" else t

instance ToSrc Discourse where
    toSrcWith o (Discourse xs) = (if punctuate o then T.unlines else T.unwords) (toSrcWith o <$> xs)
instance ToSrc DiscourseItem where
    toSrcWith o (DiSentence x) = toSrcWith o x
    toSrcWith o (DiFragment x) = toSrcWith o x
    toSrcWith o (DiFree x) = toSrcWith o x
instance ToSrc Sentence where
    toSrcWith o (Sentence sc stmt ill) =
        let t = T.unwords $ catMaybes [bold o . toSrcWith o <$> sc, Just (toSrcWith o stmt), bold o . toSrcWith o <$> ill]
        in if punctuate o then capitalizeFirst t <> "." else t
instance ToSrc Fragment where
    toSrcWith o (FrPrenex x) = toSrcWith o x
    toSrcWith o (FrTopic t) = toSrcWith o t
instance ToSrc Prenex where
    toSrcWith o (Prenex ts bi) =
        let cat = if punctuate o then T.intercalate ", " else T.unwords
        in cat (toSrcWith o <$> toList ts) <> " " <> toSrcWith o bi
instance ToSrc Statement where
    toSrcWith o (Statement mc mp preds) =
        let cat = if punctuate o then T.intercalate ": " else T.unwords
            pp = cat $ catMaybes [toSrcWith o <$> mp, Just $ toSrcWith o preds]
        in case mc of Just c -> toSrcWith o c <~> pp; Nothing -> pp
instance ToSrc PredicationC where
    toSrcWith o (Predication predicate aa nn bb) =
        let p = toSrcWith o predicate
            args = (toSrcWith o <$> aa) ++ (toSrcWith o <$> nn) ++ (toSrcWith o <$> bb)
        in if punctuate o
            then p <> "(" <> T.intercalate ", " args <> ")"
            else T.unwords (p:args)
instance ToSrc Predicate where
    toSrcWith o (Predicate vp) = toSrcWith o vp
instance ToSrc Adverbial where
    toSrcWith o (Tadvp t) = toSrcWith o t
    toSrcWith o (Tpp t) = toSrcWith o t
instance ToSrc Topic where
    toSrcWith o (Topica t) = toSrcWith o t
    toSrcWith o (Topicn t) = toSrcWith o t

instance (ToSrc t) => ToSrc (Connable' na t) where
    toSrcWith o (Conn x na ru y) =
        T.unwords [toSrcWith o x, toSrcWith o ru, toSrcWith o y]
    toSrcWith o (ConnTo to ru x to' y) =
        T.unwords [toSrcWith o to, toSrcWith o ru, toSrcWith o x, toSrcWith o to', toSrcWith o y]
    toSrcWith o (Single x) = toSrcWith o x

instance ToSrc AdvpC where
    toSrcWith o (Advp t7 vp) = toSrcWith o t7 <~> toSrcWith o vp
instance ToSrc PpC where
    toSrcWith o (Pp prep np) = toSrcWith o prep <> " " <> toSrcWith o np
instance ToSrc PrepC where
    toSrcWith o (Prep t6 vp) = toSrcWith o t6 <~> toSrcWith o vp
instance ToSrc NpC where
    toSrcWith o (Focused foc np) = toSrcWith o foc <> " " <> toSrcWith o np
    toSrcWith o (Unf np) = toSrcWith o np
instance ToSrc NpF where
    toSrcWith o (ArgRel arg rel) = toSrcWith o arg <> " " <> toSrcWith o rel
    toSrcWith o (Unr np) = toSrcWith o np
instance ToSrc NpR where
    toSrcWith o (Npro vp) = toSrcWith o vp
    toSrcWith o (Ndp dp) = toSrcWith o dp
    toSrcWith o (Ncc cc) = toSrcWith o cc
instance ToSrc Dp where
    toSrcWith o (Dp det (Just vp)) = toSrcWith o det <~> toSrcWith o vp
    toSrcWith o (Dp det Nothing) = toSrcWith o det
instance ToSrc RelC where
    toSrcWith o (Rel pred tmr) = toSrcWith o pred <> toSrcWith o tmr
instance ToSrc Cc where
    toSrcWith o (Cc pred tmr) = toSrcWith o pred <> toSrcWith o tmr
instance ToSrc VpC where
    toSrcWith o (Serial x y) = toSrcWith o x <> " " <> toSrcWith o y
    toSrcWith o (Nonserial x) = toSrcWith o x
instance ToSrc VpN where
    toSrcWith o (Vname nv name tmr) = toSrcWith o nv <> " " <> toSrcWith o name <> toSrcWith o tmr
    toSrcWith o (Vshu shu text) = toSrcWith o shu <> " " <> toSrcWith o text
    toSrcWith o (Voiv oiv np tmr) = toSrcWith o oiv <> " " <> toSrcWith o np <> " " <> toSrcWith o tmr
    toSrcWith o (Vmo mo disc teo) = T.unwords [toSrcWith o mo, toSrcWith o disc, toSrcWith o teo]
    toSrcWith o (Vlu lu stmt ky) = T.unwords [toSrcWith o lu, bracket o (toSrcWith o stmt), toSrcWith o ky]
    toSrcWith o (Vverb w) = toSrcWith o w
instance ToSrc Name where
    toSrcWith o (VerbName x) = toSrcWith o x
    toSrcWith o (TermName x) = toSrcWith o x
instance ToSrc FreeMod where
    toSrcWith o (Fint teto) = toSrcWith o teto
    toSrcWith o (Fvoc hu np) = T.unwords [toSrcWith o hu, toSrcWith o np]
    toSrcWith o (Finc ju sentence) = T.unwords [toSrcWith o ju, toSrcWith o sentence]
    toSrcWith o (Fpar kio disc ki) = T.unwords [toSrcWith o kio, toSrcWith o disc, toSrcWith o ki]
instance ToSrc (Pos t) where
    toSrcWith o (Pos _ src _) = src
instance ToSrc (W t) where
    toSrcWith o (W w fms) = T.unwords (toSrcWith o w : (toSrcWith o <$> fms))

instance ToSrc NameVerb where toSrcWith o = T.pack . show
instance ToSrc Determiner where toSrcWith o = T.pack . show
instance ToSrc Connective where toSrcWith o = T.pack . show
instance ToSrc CWord where toSrcWith o = T.pack . show
instance ToSrc Complementizer where toSrcWith o = maybe "" (toSrcWith o) . cword
instance ToSrc Terminator where
  toSrcWith o = maybe "" ((" "<>) . toSrcWith o)
