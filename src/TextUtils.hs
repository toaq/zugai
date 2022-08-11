module TextUtils where

import Data.Char
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Normalize qualified as T

-- tr "aeiou" "12345" "hello" == "h2ll4"
tr :: Text -> Text -> Text -> Text
tr froms tos = T.map (\c -> fromMaybe c $ lookup c $ T.zip froms tos)

isCombiningDiacritic :: Char -> Bool
isCombiningDiacritic c = c >= '\x0300' && c <= '\x0309'

isToaqLetter :: Char -> Bool
isToaqLetter c = isLetter c || c `T.elem` "'‘’"

isToaqChar :: Char -> Bool
isToaqChar c = isToaqLetter c || isDigit c || isCombiningDiacritic c

normalizeToaq :: Text -> Text
normalizeToaq = tr "ı‘’" "i''" . T.normalize T.NFKD . T.toLower

bareToaq :: Text -> Text
bareToaq = T.filter isToaqLetter . normalizeToaq

isToneSrc :: Text -> Bool
isToneSrc t =
  let t' = T.normalize T.NFKD t
   in T.length t' == 2 && isCombiningDiacritic (T.last t')

setTone :: Text -> Text -> Text
setTone dia t =
  case T.break (`T.elem` "aeiou") (normalizeToaq t) of
    (c, vvq) | Just (v, vq) <- T.uncons vvq -> c <> T.cons v dia <> vq
    _ -> t

copyTone :: Text -> Text -> Text
copyTone o = setTone (T.take 1 $ T.filter isCombiningDiacritic o)

inT2 :: Text -> Text
inT2 = setTone "\x0301"

combineWords :: Text -> Text -> Text
combineWords x y = if isToneSrc x then copyTone x y else T.strip (x <> " " <> y)

prettifyToaq :: Text -> Text
prettifyToaq = tr "i" "ı" . T.normalize T.NFKC

capitalizeFirst :: Text -> Text
capitalizeFirst t = T.toUpper (T.take 1 t) <> T.drop 1 t

frameDigit :: Text -> Int
frameDigit frame = maybe 0 digitToInt $ T.find isDigit frame
