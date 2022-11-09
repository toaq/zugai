{-# LANGUAGE LambdaCase #-}

module Dictionary where

import Control.Applicative
import Control.Monad
import Data.Aeson.Micro
import Data.ByteString qualified as B
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Trie qualified as Trie
import Debug.Trace
import Lex (isIllocution)
import TextUtils

data VerbClass = Tense | Aspect | Modality | Negation deriving (Eq, Ord, Show)

instance FromJSON VerbClass where
  parseJSON = withText "verb_class" $ \case
    "tense" -> pure Tense
    "aspect" -> pure Aspect
    "modality" -> pure Modality
    "negation" -> pure Negation
    t -> fail ("invalid verb_class: " <> T.unpack t)

data VerbInfo = VerbInfo
  { verbFrame :: Text,
    verbDistribution :: Text,
    verbPronominalClass :: Text,
    verbClass :: Maybe VerbClass
  }
  deriving (Eq, Show)

data Entry = Entry
  { entryToaq :: Text,
    entryType :: Text,
    entryGloss :: Text,
    entryGlossAbbreviation :: Maybe Text,
    entryVerbInfo :: Maybe VerbInfo
  }
  deriving (Eq, Show)

instance FromJSON VerbInfo where
  parseJSON = withObject "entry" $ \o ->
    VerbInfo <$> o .: "frame"
      <*> o .: "distribution"
      <*> o .: "pronominal_class"
      <*> o .:? "verb_class"

instance FromJSON Entry where
  parseJSON entry = ($ entry) $
    withObject "entry" $ \o ->
      Entry <$> o .: "toaq"
        <*> o .: "type"
        <*> o .: "gloss"
        <*> o .:? "gloss_abbreviation"
        <*> pure (parseMaybe parseJSON entry)

type Dictionary = Map Text Entry

-- lookupFrame d "poq" == Just "c"
lookupFrame :: Dictionary -> Text -> Maybe Text
lookupFrame d t = do
  entry <- d M.!? bareToaq t
  vi <- entryVerbInfo entry
  Just $ if isJust (verbClass vi) then "0" else verbFrame vi

-- lookupVerbClass d "leo" == Nothing
-- lookupVerbClass d "pu" == Just Tense
lookupVerbClass :: Dictionary -> Text -> Maybe VerbClass
lookupVerbClass d word = d M.!? bareToaq word >>= entryVerbInfo >>= verbClass

-- lookupPronoun d "poq" == Just "ho"
lookupPronoun :: Dictionary -> Text -> Maybe Text
lookupPronoun d t =
  case bareToaq <$> T.words t of
    "lu" : _ -> Just "kuy"
    x : _ -> verbPronominalClass <$> (entryVerbInfo =<< d M.!? x)
    [] -> Nothing

lookupMaxArity :: Dictionary -> Text -> Maybe Int
lookupMaxArity d word = do
    entry <- d M.!? bareToaq word
    vi <- entryVerbInfo entry
    case verbFrame vi of
        "" -> Nothing
        frame -> Just (length (T.words frame))

stripCoindexation :: Text -> Text
stripCoindexation = T.filter (not . (`T.elem` "ijx"))

dictNormalize :: Text -> Text
dictNormalize t =
  case bareToaq t of
    x | isIllocution x -> t
    x -> x

fromToadua :: Text -> (Text, Entry)
fromToadua line = (dictNormalize head, entry)
  where
    (head, gloss) = T.breakOn " " line
    entry = Entry head "verb" (T.strip gloss) Nothing Nothing

readDictionary :: IO Dictionary
readDictionary = do
  dict <- B.readFile "data/dictionary/dictionary.json"
  suppl <- B.readFile "data/supplement.json"
  extra <- T.readFile "data/toadua-glosses.txt"
  let unofficial = map fromToadua (T.lines extra)
  entries <- maybe (error "Could not parse JSON dictionary files") pure $
    liftM2 (++) (decodeStrict dict) (decodeStrict suppl)
  let official = [(dictNormalize $ entryToaq e, e) | e <- entries]
  pure $ M.fromList $ unofficial ++ official

glossWith :: Dictionary -> Text -> Text
glossWith dictionary =
  let trie = Trie.fromList [(T.encodeUtf8 k, entryGloss v) | (k, v) <- M.toList dictionary]
      forEachWord f = T.unwords . (map f) . T.words
      go :: B.ByteString -> [[Text]]
      go "" = [[]]
      go bs =
        [gloss : r | (pre, gloss, rest) <- reverse $ Trie.matches trie bs, r <- go rest]
   in forEachWord $ maybe "" (T.intercalate "-") . listToMaybe . go . T.encodeUtf8 . dictNormalize
