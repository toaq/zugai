module Lex where

import Control.Monad
import Data.Char
import Data.List.Split (wordsBy)
import Data.Text qualified as T
import Data.Text.Normalize qualified as T
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

data Tone = T2 | T3 | T4 | T5 | T6 | T7 | T8 deriving (Eq, Ord, Show)

cword :: Complementizer -> Maybe CWord
cword (CT3 c) = c
cword (CT4 c) = Just c
cword (CT5 c) = c

type Toned = (Text, Tone)
data Determiner = DT2 | Sa | Tu | Tuq | Sia | Ke | Hoi | Baq | Hi | Ja | Co | Kaga | Puy | XShi Determiner deriving (Eq, Ord, Show)
data CWord = La | Ma | Tio deriving (Eq, Ord, Show)
data Complementizer = CT3 (Maybe CWord) | CT4 CWord | CT5 (Maybe CWord) deriving (Eq, Ord, Show)
data Connective = Ra | Ri | Ru | Ro | Roi deriving (Eq, Ord, Show)
data NameVerb = Mi | Miru deriving (Eq, Ord, Show)
data Token
    = Focuser Text -- left as text so these can be turned into text<>"jeo".
    | Determiner Determiner
    | Connective Connective
    | Complementizer Complementizer
    | Oiv Text -- left as text so these can be turned into text<>"ga".
    | NameVerb NameVerb
    | Shu
    | Mo
    | Lu
    | Bi | Cy | Ga | Hu | Ju | Ki | Kio | Ky | Na | Teo | To
    | Illocution Toned -- not interpreted
    | SentenceConnector Text -- not interpreted
    | Interjection Toned -- not interpreted
    | Verb Text
    | Pronoun Text -- implies natural t2
    | T4jei -- e.g. süq is tokenized as [Complementizer (CT3 Nothing), T4jei, Pronoun "suq"]
    | T6token
    | T7token
    deriving (Eq, Ord, Show)

-- A token (or other inner type) tagged with source position and source text.
data Pos t = Pos { posPos :: SourcePos, posSrc :: Text, posVal :: t } deriving (Eq)
instance Functor Pos where
    fmap f (Pos x y z) = Pos x y (f z)
instance Show t => Show (Pos t) where
    show x = show (posSrc x)
    -- show x = show (posVal x) ++ "\x1b[96m~" ++ T.unpack (posSrc x) ++ "\x1b[0m" -- "\x1b[32m" ++ T.unpack (posSrc x) ++ "\x1b[0m"

convertDigits :: Text -> Text
convertDigits t =
    case T.unsnoc t of
        Just (t', c) | isDigit c && t' /= "" && T.all isToaqLetter t' ->
            case c of
                '2' -> setTone "\x0301" t'
                '3' -> setTone "\x0308" t'
                '4' -> setTone "\x0309" t'
                '5' -> setTone "\x0302" t'
                '6' -> setTone "\x0300" t'
                '7' -> setTone "\x0303" t'
                '8' -> setTone "" t'
                _ -> error ("Invalid tone digit: " <> show c)
        _ -> t

toneFromChar :: Char -> Maybe Tone
toneFromChar '\x0301' = Just T2
toneFromChar '\x0308' = Just T3
toneFromChar '\x0309' = Just T4
toneFromChar '\x0302' = Just T5
toneFromChar '\x0300' = Just T6
toneFromChar '\x0303' = Just T7
toneFromChar _ = Nothing

isPronoun :: Text -> Bool
isPronoun = (`elem` T.words "ji suq nhao suo muy miy may kou ray ho maq hoq ta rou kuy ze fuy bou aq cheq")

isOiv :: Text -> Bool
isOiv = (`elem` T.words "po jei mea")

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

toCWord :: Text -> Maybe CWord
toCWord "la" = Just La
toCWord "ma" = Just Ma
toCWord "tio" = Just Tio
toCWord _ = Nothing

toneTokens :: Tone -> [Token]
toneTokens T2 = [Determiner DT2]
toneTokens T3 = [Complementizer $ CT3 Nothing]
toneTokens T4 = []
toneTokens T5 = [Complementizer $ CT5 Nothing]
toneTokens T6 = [T6token]
toneTokens T7 = [T7token]
toneTokens T8 = []

toToken :: LexOptions -> String -> Either String [Token]
toToken opt word =
    let base = T.pack (filter isLetter word)
        tone' = msum (map toneFromChar word)
        defaultTone = if allowSparseToneMarking opt then T4 else T8
        tone = maybe defaultTone id tone'
    in case () of
        _ | Just token <- toToneless base ->
            if tone' == Just T8 || tone' == Nothing
                then Right [token]
                else Left (T.unpack base <> " must have neutral tone")
        _ | Just cword <- toCWord base, tone == T3 -> Right [Complementizer $ CT3 (Just cword)] -- lä mä tïo
        _ | Just cword <- toCWord base, tone == T4 -> Right [Complementizer $ CT4 cword       ] -- la ma tio
        _ | Just cword <- toCWord base, tone == T5 -> Right [Complementizer $ CT5 (Just cword)] -- lâ mâ tîo
        _ | isIllocution base -> Right [Illocution (base, tone)]
        _ | isInterjection base -> Right [Interjection (base, tone)]
        _ | isPronoun base, tone == T2 -> Right [Pronoun base]
        _ | isPronoun base -> Right $ toneTokens tone ++ [T4jei, Pronoun base]
        _ | otherwise -> Right $ (toneTokens tone ++) . pure $ case base of
            _ | isOiv base -> Oiv base
            "mo" -> Mo
            "lu" -> Lu
            "shu" -> Shu
            "mi" -> NameVerb Mi
            "miru" -> NameVerb Miru
            _ -> Verb base

makeSuprasegmental :: SourcePos -> Token -> Pos Token
makeSuprasegmental pos t@(Determiner DT2) = Pos pos "◌́" t
makeSuprasegmental pos t@(Complementizer (CT3 Nothing)) = Pos pos "◌̈" t
makeSuprasegmental pos t@(T4jei) = Pos pos "◌̉" t
makeSuprasegmental pos t@(Complementizer (CT5 Nothing)) = Pos pos "◌̂" t
makeSuprasegmental pos t@(T6token) = Pos pos "◌̀" t
makeSuprasegmental pos t@(T7token) = Pos pos "◌̃" t

tokenParser :: LexOptions -> Parsec Text () [Pos Token]
tokenParser opt = do
    pos <- getPosition
    text <- T.pack <$> many1 (satisfy isToaqChar)
    let src = T.normalize T.NFKC $ convertDigits text
    case toToken opt $ T.unpack $ normalizeToaq src of
        Right [] -> error "wtf"
        Right [token] -> pure [Pos pos src token]
        Right tokens -> pure $
            map (makeSuprasegmental pos) (init tokens)
            ++ [Pos pos (bareToaq src) $ last tokens]
        Left err -> fail err

trivia :: Parsec Text () ()
trivia = skipMany (satisfy $ not . isToaqChar)

lexToaqOpt :: LexOptions -> Text -> Either ParseError [Pos Token]
lexToaqOpt opt text =
    parse (concat <$> many1 (trivia *> tokenParser opt <* trivia)) "" text

lexToaq :: Text -> Either ParseError [Pos Token]
lexToaq = lexToaqOpt defaultLexOptions
