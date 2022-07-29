{-# LANGUAGE LambdaCase #-}
module Dictionary where

import Control.Applicative
import Control.Monad
import Data.ByteString qualified as B
import Data.Aeson.Micro
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Encoding qualified as T
import Data.Trie qualified as Trie
import Debug.Trace

import Lex (isIllocution)
import TextUtils

data Distribution = Distributive | NonDistributive deriving (Eq, Ord, Show)

parseDistribution :: Text -> [Distribution]
parseDistribution = concatMap (\case 'd' -> [Distributive]; 'n' -> [NonDistributive]; _ -> []) . T.unpack

data VerbInfo =
    VerbInfo
        { verbFrame :: Text
        , verbDistribution :: Text
        , verbPronominalClass :: Text }
    deriving (Eq, Show)

data Entry =
    Entry
        { entryToaq :: Text
        , entryType :: Text
        , entryGloss :: Text
        , entryVerbInfo :: Maybe VerbInfo }
    deriving (Eq, Show)

instance FromJSON VerbInfo where
    parseJSON = withObject "entry" $ \o ->
        VerbInfo <$> o .: "frame" <*> o .: "distribution" <*> o .: "pronominal_class"

instance FromJSON Entry where
    parseJSON entry = ($entry) $ withObject "entry" $ \o ->
        Entry <$> o .: "toaq" <*> o .: "type" <*> o .: "gloss" <*> pure (parseMaybe parseJSON entry)

type Dictionary = Map Text Entry

-- lookupFrame d "poq" == Just "ho"
lookupFrame :: Dictionary -> Text -> Maybe Text
lookupFrame d t = verbFrame <$> (entryVerbInfo =<< d M.!? bareToaq t)

-- lookupPronoun d "poq" == Just "ho"
lookupPronoun :: Dictionary -> Text -> Maybe Text
lookupPronoun d t =
    case bareToaq <$> T.words t of
        "lu":_ -> Just "kuy"
        x:_ -> verbPronominalClass <$> (entryVerbInfo =<< d M.!? x)
        [] -> Nothing

glossNormalize :: Text -> Text
glossNormalize t =
    case bareToaq t of
        x | isIllocution x -> t
        x -> x

readDictionary :: IO Dictionary
readDictionary = do
    dict <- B.readFile "data/dictionary/dictionary.json"
    extra <- T.readFile "data/toadua-glosses.txt"
    let unofficial = [(glossNormalize head, Entry head "verb" (T.strip gloss) Nothing) | line <- T.lines extra, let (head,gloss) = T.breakOn " " line]
    let Just entries = decodeStrict dict :: Maybe [Entry]
    let official = [(glossNormalize $ entryToaq e, e) | e <- entries]
    pure $ M.fromList $ unofficial ++ official

glossWith :: Dictionary -> Text -> Text
glossWith dictionary =
    let
        trie = Trie.fromList [(T.encodeUtf8 k, entryGloss v) | (k,v) <- M.toList dictionary]
        go :: B.ByteString -> [[Text]]
        go "" = [[]]
        go bs =
            [gloss:r | (pre, gloss, rest) <- reverse $ Trie.matches trie bs, r <- go rest]
    in
        maybe "" (T.intercalate "-") . listToMaybe . go . T.encodeUtf8 . glossNormalize
