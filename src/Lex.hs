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

data Determiner = Sa | Tu | Tuq | Tushi | Sia | Ke | Hoi | Baq | Hi | Ja deriving (Eq, Ord, Show)
data Complementizer = La | Ma | Tio deriving (Eq, Ord, Show)
data Connective = Ra | Ri | Ru | Ro | Roi deriving (Eq, Ord, Show)
data NameVerb = Mi | Miru deriving (Eq, Ord, Show)
data Token
    = Focuser Text -- left as text so these can be turned into text<>"jeo".
    | Determiner Determiner
    | Connective Connective
    | Complementizer Complementizer Tone
    | Oiv Text Tone -- left as text so these can be turned into text<>"ga".
    | NameVerb NameVerb Tone
    | Ga
    | Shu Tone
    | Mo Tone
    | Teo
    | Lu Tone
    | Ky
    | Bi | Cy | Ju | Ki | Kio | To
    | Illocution Text Tone -- not interpreted
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

isFocuser :: Text -> Bool
isFocuser = (`elem` T.words "ku bei juaq mao tou")

isInterjection :: Text -> Bool
isInterjection word = word `elem` T.words "ifu aja ahi ume ufu a ua obe upa buzy oai ubai eni aiba obe e nho zi jadi kiji jiki"

isIllocution :: Text -> Bool
isIllocution = (`elem` T.words "da ka moq ba nha shou go zay")

isSentenceConnector :: Text -> Bool
isSentenceConnector = (`elem` T.words "je keo tiu nhu fou")

toToneless :: Text -> Maybe Token
toToneless "sa" = Just (Determiner Sa)
toToneless "tu" = Just (Determiner Tu)
toToneless "tuq" = Just (Determiner Tuq)
toToneless "tushi" = Just (Determiner Tushi)
toToneless "sia" = Just (Determiner Sia)
toToneless "ke" = Just (Determiner Ke)
toToneless "hoi" = Just (Determiner Hoi)
toToneless "baq" = Just (Determiner Baq)
toToneless "hi" = Just (Determiner Hi)
toToneless "ja" = Just (Determiner Ja)
toToneless "ra" = Just (Connective Ra)
toToneless "ri" = Just (Connective Ri)
toToneless "ru" = Just (Connective Ru)
toToneless "ro" = Just (Connective Ro)
toToneless "roi" = Just (Connective Roi)
toToneless "ga" = Just Ga
toToneless "teo" = Just Teo
toToneless "ky" = Just Ky
toToneless "bi" = Just Bi
toToneless "cy" = Just Cy
toToneless "ju" = Just Ju
toToneless "ki" = Just Ki
toToneless "kio" = Just Kio
toToneless "to" = Just To
toToneless x | isFocuser x = Just (Focuser x)
toToneless x | isSentenceConnector x = Just (SentenceConnector x)
toToneless _ = Nothing

toToken :: String -> Either Text Token
toToken word =
    let base = T.pack (filter isAlpha word)
        tone' = msum (map toneFromChar word)
        tone = maybe T4 id tone'
    in case base of
        _ | Just token <- toToneless base ->
            if tone' == Just T8 || tone' == Nothing
                then Right token
                else Left (base <> " must have neutral tone")
        "la" -> Right (Complementizer La tone)
        "ma" -> Right (Complementizer Ma tone)
        "tio" -> Right (Complementizer Tio tone)
        "po" -> Right (Oiv "po" tone)
        "jei" -> Right (Oiv "jei" tone)
        "mea" -> Right (Oiv "mea" tone)
        "mo" -> Right (Mo tone)
        "lu" -> Right (Lu tone)
        "shu" -> Right (Shu tone)
        "mi" -> Right (NameVerb Mi tone)
        "miru" -> Right (NameVerb Miru tone)
        _ | isIllocution base -> Right (Illocution base tone)
        _ | isInterjection base -> Right (Interjection base tone)
        _ -> Right (Verb base tone)

lexer :: Text -> Either Text [Token]
lexer text =
    let clean = T.replace "ı" "i" $ T.normalize T.NFKD $ T.toLower text
        words = wordsBy (not . isToaqChar) (T.unpack clean)
    in sequence $ map toToken words
