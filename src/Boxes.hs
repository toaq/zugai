module Boxes where

import Prelude hiding (div)
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Lex
import Parse
import ToSrc
import TextUtils

(<~>) :: Text -> Text -> Text
(<~>) = combineWords

class ToBoxes a where
    toBoxes :: a -> Text

div :: Text -> [Text] -> Text
div className [] = ""
div className children = "<div class=\"box " <> className <> "\">" <> T.concat children <> "</div>"

div1 :: Text -> Text -> Text
div1 className child = div className [child]

leaf :: ToSrc a => a -> Text
leaf = div1 "leaf" . prettifyToaq . toSrc

instance ToBoxes Discourse where
    toBoxes (Discourse xs) = div "discourse" (toBoxes <$> xs)
instance ToBoxes DiscourseItem where
    toBoxes (DiSentence x) = toBoxes x
    toBoxes (DiFragment x) = toBoxes x
    toBoxes (DiFree x) = leaf x
instance ToBoxes Sentence where
    toBoxes (Sentence sc stmt ill) =
        div "sentence" $ catMaybes [div1 "sentence-boundary" . leaf <$> sc, Just (toBoxes stmt), div1 "illocution" . leaf <$> ill]
instance ToBoxes Fragment where
    toBoxes (FrPrenex x) = toBoxes x
    toBoxes (FrTopic x) = toBoxes x
instance ToBoxes Prenex where
    toBoxes (Prenex ts bi) =
        div "prenex" $ (toBoxes <$> toList ts) <> [leaf bi]
instance ToBoxes Statement where
    toBoxes (Statement mc mp preds) =
        div "clause" $ catMaybes [leaf <$> mc, toBoxes <$> mp, Just $ toBoxes preds]
instance ToBoxes PredicationC where
    toBoxes (Predication predicate aa nn bb) =
        div1 "verbal-complex" (toBoxes predicate) <> div "post-field" ((toBoxes <$> aa) ++ (toBoxes <$> nn) ++ (toBoxes <$> bb))
instance ToBoxes Predicate where
    toBoxes (Predicate vp) = leaf vp
instance ToBoxes Adverbial where
    toBoxes (Tadvp t) = div1 "adverbial" $ leaf t
    toBoxes (Tpp t) = div1 "adverbial" $ leaf t
instance ToBoxes Topic where
    toBoxes (Topica t) = toBoxes t
    toBoxes (Topicn t) = toBoxes t

instance (ToBoxes t) => ToBoxes (Connable' na t) where
    toBoxes (Conn x na ru y) = div "coordinaton" [toBoxes x, leaf ru, toBoxes y]
    toBoxes (ConnTo to ru x to' y) = div "coordinaton" [leaf to, leaf ru, toBoxes x, leaf to', toBoxes y]
    toBoxes (Single x) = toBoxes x

instance ToBoxes NpC where
    toBoxes (Focused foc np) = leaf foc <> toBoxes np
    toBoxes (Unf np) = toBoxes np
instance ToBoxes NpF where
    toBoxes (ArgRel arg rel) = div "argument" [leaf arg, toBoxes rel]
    toBoxes (Unr np) = div "argument" [leaf np]
instance ToBoxes RelC where
    toBoxes (Rel pred tmr) = toBoxes pred <> leaf tmr
