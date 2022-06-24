module Lex where

import Control.Monad
import Data.Char
import Data.List.Split (wordsBy)
import Data.Text qualified as T
import Data.Text (Text)
import Data.Text.Normalize qualified as T

data Tone
    = T2 | T3 | T4 | T5 | T6 | T7 | T8
    deriving (Eq, Ord, Show)

-- TODO: use this cooler type instead
data Focuser = Ku | Bei | Juq | Mao | Tou deriving (Eq, Ord, Show)
data Determiner = Sa | Tu | Tuq | Tushi | Sia | Ke | Hoi | Baq | Hi | Ja deriving (Eq, Ord, Show)
data Complementizer = La | Ma | Tio deriving (Eq, Ord, Show)
data Connective = Ra | Ri | Ru | Ro | Roi deriving (Eq, Ord, Show)
data Oiv = Po | Jei | Mea deriving (Eq, Ord, Show)
data Token
    = Focuser Focuser
    | Determiner Determiner
    | Connective Connective
    | Complementizer Complementizer Tone
    | Oiv Oiv
    | Ga
    | Mo Tone
    | Teo
    | Lu Tone
    | Ky
    | Bi | Cy | Ju | Ki | Kio | To
    | Illocution Text -- not interpreted
    | SentenceConnector Text -- not interpreted
    | Interjection Text Tone -- not interpreted
    | Verb Text Tone
    deriving (Eq, Ord, Show)

toneFromChar :: Char -> Maybe Tone
toneFromChar '2' = Just T2
toneFromChar '3' = Just T3
toneFromChar '4' = Just T4
toneFromChar '5' = Just T5
toneFromChar '6' = Just T6
toneFromChar '7' = Just T7
toneFromChar '8' = Just T8
toneFromChar '\x0301' = Just T2
toneFromChar '\x0308' = Just T3
toneFromChar '\x0309' = Just T4
toneFromChar '\x0302' = Just T5
toneFromChar '\x0300' = Just T6
toneFromChar '\x0303' = Just T7
toneFromChar _ = Nothing

isToaqChar :: Char -> Bool
isToaqChar c =
    c >= 'a' && c <= 'z' || c >= '2' && c <= '8' || c >= '\x0300' && c <= '\x030f'

isInterjection :: Text -> Bool
isInterjection word = word `elem` T.words "ifu aja ahi ume ufu a ua obe upa buzy oai ubai eni aiba obe e nho zi jadi kiji jiki"

isToneless :: Text -> Bool
isToneless word = isInterjection word || word `elem` T.words "ku bei juq mao tou  sa tu tuq tushi sia ke hoi baq hi ja  je keo tiu nhu fou  da ka moq ba nha shou go zay  ra ri ru ro roi  bi cy ga ju ki kio ky teo"

splitWord :: String -> (Text, Tone)
splitWord word =
    let base = T.pack (filter isAlpha word)
        firstTone = msum (map toneFromChar word)
        tone = maybe (if isToneless base then T8 else T4) id firstTone
    in (base, tone)

-- TODO: further conversion to even better and cooler token type. and then eliminate `isToneless`
-- toToken :: (Text, Tone) -> Either Text Token
-- toToken ("ku", T8) = Just (Focuser Ku)
-- toToken ("bei", T8) = Just (Focuser Bei)
-- toToken ("juq", T8) = Just (Focuser Juq)
-- toToken ("mao", T8) = Just (Focuser Mao)
-- toToken ("tou", T8) = Just (Focuser Tou)

lex :: Text -> [(Text, Tone)]
lex text =
    let clean = T.replace "ı" "i" $ T.normalize T.NFKD $ T.toLower text
        words = wordsBy (not . isToaqChar) (T.unpack clean)
    in map splitWord words
