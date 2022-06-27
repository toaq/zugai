module TextUtils where

import Data.Char
import Data.Text qualified as T
import Data.Text (Text)
import Data.Text.Normalize qualified as T

-- tr "aeiou" "12345" "hello" == "h2ll4"
tr :: Text -> Text -> Text -> Text
tr froms tos = T.map (\c -> maybe c id $ lookup c $ T.zip froms tos)

isToaqChar :: Char -> Bool
isToaqChar c =
    isLetter c
    || c == 'ı'
    || c `T.elem` "'‘’"
    || c >= '2' && c <= '8'
    || c >= '\x0300' && c <= '\x0309'

normalizeToaq :: Text -> Text
normalizeToaq = tr "ı‘’" "i''" . T.normalize T.NFKD . T.toLower

bareToaq :: Text -> Text
bareToaq = T.filter isLetter . normalizeToaq