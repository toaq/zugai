module Boxes where

import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Parse
import TextUtils (combineWords, prettifyToaq)
import ToSrc
import Prelude hiding (div)

(<~>) :: Text -> Text -> Text
(<~>) = combineWords

data BoxesType = BoxesSimple | BoxesDetailed

class ToBoxes a where
  toBoxes :: BoxesType -> a -> Text

div :: Text -> [Text] -> Text
div className [] = ""
div className children = "<div class=\"box " <> className <> "\">" <> T.concat children <> "</div>"

div1 :: Text -> Text -> Text
div1 className child = div className [child]

leaf :: ToSrc a => a -> Text
leaf = div1 "leaf" . prettifyToaq . toSrc

handleArg :: (ToSrc a, ToBoxes a) => BoxesType -> a -> Text
handleArg    BoxesSimple   = leaf
handleArg bt@BoxesDetailed = toBoxes bt

instance ToBoxes Discourse where
  toBoxes bt (Discourse xs) = div "discourse" (toBoxes bt <$> xs)

instance ToBoxes DiscourseItem where
  toBoxes bt (DiSentence x) = toBoxes bt x
  toBoxes bt (DiFragment x) = toBoxes bt x
  toBoxes _  (DiFree x) = leaf x

instance ToBoxes Sentence where
  toBoxes bt (Sentence sc stmt ill) = div "sentence" $ catMaybes
    [ div1 "sentence-boundary" . leaf <$> sc,
      Just (toBoxes bt stmt),
      div1 "illocution" . leaf <$> ill
    ]

instance ToBoxes Fragment where
  toBoxes bt (FrPrenex x) = toBoxes bt x
  toBoxes bt (FrTopic x) = toBoxes bt x

instance ToBoxes Prenex where
  toBoxes bt (Prenex ts bi) =
    div "topic" $ (toBoxes bt <$> toList ts) <> [leaf bi]

instance ToBoxes Statement where
  toBoxes bt (Statement mc mp preds) =
    div "clause" $ catMaybes
      [ leaf <$> mc,
        toBoxes bt <$> mp,
        Just $ toBoxes bt preds
      ]

instance ToBoxes PredicationC where
  toBoxes bt (Predication predicate aa nn bb) =
    let postField =
          (toBoxes bt <$> aa) ++ (toBoxes bt <$> nn) ++ (toBoxes bt <$> bb)
    in div1 "verbal-complex" (toBoxes bt predicate)
       <> div "post-field" postField

instance ToBoxes Predicate where
  toBoxes _ (Predicate vp) = leaf vp

instance ToBoxes Adverbial where
  toBoxes _ (Tadvp t) = div1 "adverbial" $ leaf t
  toBoxes _ (Tpp t) = div1 "adverbial" $ leaf t

instance ToBoxes Topic where
  toBoxes bt (Topica t) = toBoxes bt t
  toBoxes bt (Topicn t) = toBoxes bt t

instance (ToBoxes t) => ToBoxes (Connable' na t) where
  toBoxes bt (Conn x na ru y) = div "coordinaton" [toBoxes bt x, leaf ru, toBoxes bt y]
  toBoxes bt (ConnTo to ru x to' y) = div "coordinaton" [leaf to, leaf ru, toBoxes bt x, leaf to', toBoxes bt y]
  toBoxes bt (Single x) = toBoxes bt x

instance ToBoxes NpC where
  toBoxes bt (Focused foc np) = leaf foc <> toBoxes bt np
  toBoxes bt (Unf np) = toBoxes bt np

instance ToBoxes NpF where
  toBoxes BoxesSimple node@(ArgRel _ _) = div1 "argument" $ leaf node
  toBoxes bt@BoxesDetailed (ArgRel arg rel) = div "argument" [handleArg bt arg, toBoxes bt rel]
  toBoxes bt (Unr np) = div "argument" [handleArg bt np]

instance ToBoxes NpR where
  toBoxes bt (Npro p) = leaf p
  toBoxes bt (Ndp dp) = leaf dp
  toBoxes bt (Ncc cc) = toBoxes bt cc

-- data NpR = Npro (W Text) | Ndp Dp | Ncc Cc deriving (Eq, Show)

instance ToBoxes Cc where
  -- you can tell my L keycap's fallen off lol
  toBoxes bt (Cc stmt cy) = div "complementizer" $ catMaybes
    [ Just $ toBoxes bt stmt,
      leaf <$> cy
    ]

instance ToBoxes RelC where
  toBoxes bt (Rel pred tmr) = toBoxes bt pred <> leaf tmr
