module Lex where

import Control.Monad
import Data.Char
import Data.List.Split (wordsBy)
import Data.Text qualified as T
import Data.Text (Text)
import Text.Parsec as P
import TextUtils

data LexOptions =
    LexOptions
    { allowSparseToneMarking :: Bool
    } deriving (Eq, Show)

defaultLexOptions :: LexOptions
defaultLexOptions =
    LexOptions
    { allowSparseToneMarking = True
    }

data Tone
    = T2 | T3 | T4 | T5 | T6 | T7 | T8
    deriving (Eq, Ord, Show)

type Toned = (Text, Tone)
data Determiner = Sa | Tu | Tuq | Sia | Ke | Hoi | Baq | Hi | Ja | Co | Kaga | Puy | XShi Determiner deriving (Eq, Ord, Show)
data Complementizer = La | Ma | Tio deriving (Eq, Ord, Show)
data Connective = Ra | Ri | Ru | Ro | Roi deriving (Eq, Ord, Show)
data NameVerb = Mi | Miru deriving (Eq, Ord, Show)
data Token
    = Focuser Text -- left as text so these can be turned into text<>"jeo".
    | Determiner Determiner
    | Connective Connective
    | Complementizer Complementizer Tone
    | Oiv Toned -- left as text so these can be turned into text<>"ga".
    | NameVerb NameVerb Tone
    | Shu Tone
    | Mo Tone
    | Lu Tone
    | Bi | Cy | Ga | Hu | Ju | Ki | Kio | Ky | Na | Teo | To
    | Illocution Toned -- not interpreted
    | SentenceConnector Text -- not interpreted
    | Interjection Toned -- not interpreted
    | Verb Toned
    deriving (Eq, Ord, Show)

-- A token (or other inner type) tagged with source position and source text.
data Pos t = Pos { posPos :: SourcePos, posSrc :: Text, posVal :: t } deriving (Eq)
instance Functor Pos where
    fmap f (Pos x y z) = Pos x y (f z)
instance Show t => Show (Pos t) where
    show x = show (posSrc x)
    -- show x = show (posVal x) ++ "\x1b[96m~" ++ T.unpack (posSrc x) ++ "\x1b[0m" -- "\x1b[32m" ++ T.unpack (posSrc x) ++ "\x1b[0m"

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

isFocuser :: Text -> Bool
isFocuser = (`elem` T.words "ku bei juaq mao tou")

isInterjection :: Text -> Bool
isInterjection word = word `elem` T.words "ifu aja ahi ume ufu a ua obe upa buzy oai ubai eni aiba obe e nho zi jadi kiji jiki haha m"

isIllocution :: Text -> Bool
isIllocution = (`elem` T.words "da ka moq ba nha shou go zay")

isSentenceConnector :: Text -> Bool
isSentenceConnector = (`elem` T.words "je keo tiu nhu fou")

toToneless :: Text -> Maybe Token
toToneless "sa" = Just (Determiner Sa)
toToneless "tu" = Just (Determiner Tu)
toToneless "tuq" = Just (Determiner Tuq)
toToneless "sia" = Just (Determiner Sia)
toToneless "ke" = Just (Determiner Ke)
toToneless "hoi" = Just (Determiner Hoi)
toToneless "baq" = Just (Determiner Baq)
toToneless "hi" = Just (Determiner Hi)
toToneless "ja" = Just (Determiner Ja)
toToneless "co" = Just (Determiner Co)
toToneless "kaga" = Just (Determiner Kaga)
toToneless "puy" = Just (Determiner Puy)
toToneless "ra" = Just (Connective Ra)
toToneless "ri" = Just (Connective Ri)
toToneless "ru" = Just (Connective Ru)
toToneless "ro" = Just (Connective Ro)
toToneless "roi" = Just (Connective Roi)
toToneless "bi" = Just Bi
toToneless "cy" = Just Cy
toToneless "ga" = Just Ga
toToneless "hu" = Just Hu
toToneless "ju" = Just Ju
toToneless "ki" = Just Ki
toToneless "kio" = Just Kio
toToneless "ky" = Just Ky
toToneless "na" = Just Na
toToneless "teo" = Just Teo
toToneless "to" = Just To
toToneless x
    | Just inner <- T.stripSuffix "shi" x
    , not ("shi" `T.isSuffixOf` inner)
    , Just (Determiner det) <- toToneless inner
    = Just (Determiner $ XShi det)
toToneless x | isFocuser x = Just (Focuser x)
toToneless x | isSentenceConnector x = Just (SentenceConnector x)
toToneless _ = Nothing

toToken :: LexOptions -> String -> Either String Token
toToken opt word =
    let base = T.pack (filter isLetter word)
        tone' = msum (map toneFromChar word)
        defaultTone = if allowSparseToneMarking opt then T4 else T8
        tone = maybe defaultTone id tone'
    in case base of
        _ | Just token <- toToneless base ->
            if tone' == Just T8 || tone' == Nothing
                then Right token
                else Left (T.unpack base <> " must have neutral tone")
        "la" -> Right (Complementizer La tone)
        "ma" -> Right (Complementizer Ma tone)
        "tio" -> Right (Complementizer Tio tone)
        "po" -> Right (Oiv ("po", tone))
        "jei" -> Right (Oiv ("jei", tone))
        "mea" -> Right (Oiv ("mea", tone))
        "mo" -> Right (Mo tone)
        "lu" -> Right (Lu tone)
        "shu" -> Right (Shu tone)
        "mi" -> Right (NameVerb Mi tone)
        "miru" -> Right (NameVerb Miru tone)
        _ | isIllocution base -> Right (Illocution (base, tone))
        _ | isInterjection base -> Right (Interjection (base, tone))
        _ -> Right (Verb (base, tone))

tokenParser :: LexOptions -> Parsec Text () (Pos Token)
tokenParser opt = do
    pos <- getPosition
    text <- T.pack <$> many1 (satisfy isToaqChar)
    case toToken opt $ T.unpack $ normalizeToaq text of
        Right token -> pure (Pos pos text token)
        Left err -> fail err

trivia :: Parsec Text () ()
trivia = skipMany (satisfy $ not . isToaqChar)

lexToaqOpt :: LexOptions -> Text -> Either ParseError [Pos Token]
lexToaqOpt opt text =
    parse (many1 (trivia *> tokenParser opt <* trivia)) "" text

lexToaq :: Text -> Either ParseError [Pos Token]
lexToaq = lexToaqOpt defaultLexOptions

unr :: Either a b -> b
unr (Right b) = b