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
import Data.Text.Encoding qualified as T
import Data.Trie qualified as Trie

import Lex (isIllocution)
import TextUtils

data SlotType = Concrete | Nary Int deriving (Eq, Ord, Show)

parseSlotType :: Char -> Maybe SlotType
parseSlotType 'c' = Just (Concrete)
parseSlotType '0' = Just (Nary 0)
parseSlotType '1' = Just (Nary 1)
parseSlotType '2' = Just (Nary 2)
parseSlotType _ = Nothing

parseFrame :: Text -> [SlotType]
parseFrame = catMaybes . map parseSlotType . T.unpack

data Distribution = Distributive | NonDistributive deriving (Eq, Ord, Show)

parseDistribution :: Text -> [Distribution]
parseDistribution = concatMap (\case 'd' -> [Distributive]; 'n' -> [NonDistributive]; _ -> []) . T.unpack

data PronominalClass
    = HoClass -- I
    | MaqClass -- II 
    | HoqClass -- III
    | TaClass -- IV
    | RouClass -- V
    | KuyClass -- VI
    | ZeClass -- VII
    | FuyClass -- VIII
    | BouClass -- IX
    | RaiClass -- 0
    deriving (Eq, Ord, Show)

data VerbInfo = VerbInfo { verbSlotType :: [SlotType], verbDistribution :: [Distribution], verbPronominalClass :: Text } deriving (Eq, Show)
data Entry = Entry { entryToaq :: Text, entryType :: Text, entryGloss :: Text, entryVerbInfo :: Maybe VerbInfo } deriving (Eq, Show)

instance FromJSON VerbInfo where
    parseJSON = withObject "entry" $ \o ->
        VerbInfo <$> fmap parseFrame (o .: "frame") <*> fmap parseDistribution (o .: "distribution") <*> o .: "pronominal_class"

instance FromJSON Entry where
    parseJSON entry = ($entry) $ withObject "entry" $ \o ->
        Entry <$> o .: "toaq" <*> o .: "type" <*> o .: "gloss" <*> pure (parseMaybe parseJSON entry)

type Dictionary = Map Text Entry

glossNormalize :: Text -> Text
glossNormalize t =
    case bareToaq t of
        x | isIllocution x -> t
        x -> x

readDictionary :: IO Dictionary
readDictionary = do
    dict <- B.readFile "dictionary/dictionary.json"
    let Just entries = decodeStrict dict :: Maybe [Entry]
    pure $ M.fromList [(glossNormalize $ entryToaq e, e) | e <- entries]

glossWith :: Dictionary -> Text -> Text
glossWith dictionary =
    let
        trie = Trie.fromList [(T.encodeUtf8 k, entryGloss v) | (k,v) <- M.toList dictionary]
        go :: B.ByteString -> [[Text]]
        go "" = [[]]
        go bs =
            [gloss:r | (pre, gloss, rest) <- reverse $ Trie.matches trie bs, r <- go rest]
    in
        maybe "???" (T.intercalate "-") . listToMaybe . go . T.encodeUtf8 . glossNormalize
