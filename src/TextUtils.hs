module TextUtils where

import Data.Char
import Data.Text qualified as T
import Data.Text (Text)
import Data.Text.Normalize qualified as T

-- tr "aeiou" "12345" "hello" == "h2ll4"
tr :: Text -> Text -> Text -> Text
tr froms tos = T.map (\c -> maybe c id $ lookup c $ T.zip froms tos)

isCombiningDiacritic :: Char -> Bool
isCombiningDiacritic c = c >= '\x0300' && c <= '\x0309'

isToaqChar :: Char -> Bool
isToaqChar c =
    isLetter c
    || c == 'ı'
    || c `T.elem` "'‘’"
    || c >= '2' && c <= '8'
    || isCombiningDiacritic c

normalizeToaq :: Text -> Text
normalizeToaq = tr "ı‘’" "i''" . T.normalize T.NFKD . T.toLower

bareToaq :: Text -> Text
bareToaq = T.filter isLetter . normalizeToaq

isToneSrc :: Text -> Bool
isToneSrc t = T.length t == 2 && isCombiningDiacritic (T.last t)

copyTone :: Text -> Text -> Text
copyTone o t =
    let dia = T.take 1 $ T.filter isCombiningDiacritic o
    in case T.break (`T.elem` "aeiou") (normalizeToaq t) of
        (c, vvq) | Just (v, vq) <- T.uncons vvq -> T.normalize T.NFKC $ c <> T.cons v dia <> vq
        _ -> t

inT2 :: Text -> Text
inT2 = copyTone "ó"