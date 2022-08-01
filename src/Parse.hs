module Parse where
import Control.Monad
import Lex
import Data.List.NonEmpty qualified as N
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import Data.Text (Text)
import Debug.Trace
import Text.Parsec as P

-- Most leaves of the parse tree are "W" (words with possible free modifiers after them).
data W t = W (Pos t) [FreeMod] deriving (Eq, Functor)

unW :: W t -> t
unW (W (Pos _ _ t) _) = t

instance Show t => Show (W t) where
    show (W (Pos p s v) fs) = show (T.unpack s) ++ ['#' | _ <- fs]

data FreeMod
    = Fint (Pos Toned)
    | Fvoc (Pos () {-hu-}) Np
    | Finc (Pos () {-ju-}) Sentence
    | Fpar (Pos () {-kio-}) Discourse (W () {-ki-})
    deriving (Eq, Show)

-- Generic type for constructs which can be connected like "X (na) ru Y", or "to ru X to Y", or occur alone.
data Connable' na c
    = Conn c na (W Connective) (Connable' na c)
    | ConnTo (W () {-to-}) (W Connective) (Connable' na c) (W () {-to-}) (Connable' na c)
    | Single c
    deriving (Eq, Show)
type Connable = Connable' ()
type ConnableNa = Connable' (W ())

-- Parse tree types
newtype Discourse = Discourse [DiscourseItem] deriving (Eq, Show)
data DiscourseItem = DiSentence Sentence | DiFragment Fragment | DiFree FreeMod deriving (Eq, Show)
data Sentence = Sentence (Maybe (W Text {-je-})) Statement (Maybe (W Toned {-da-})) deriving (Eq, Show)
data Fragment = FrPrenex Prenex | FrTopic Topic deriving (Eq, Show)
data Topic = Topica Adverbial | Topicn Np deriving (Eq, Show)
data Prenex = Prenex (NonEmpty Topic) (W () {-bi-}) deriving (Eq, Show)
data Statement = Statement (Maybe (W Complementizer)) (Maybe Prenex) Predication deriving (Eq, Show)
type Predication = ConnableNa PredicationC
data PredicationC = Predication Predicate [Adverbial] [Np] [Adverbial] deriving (Eq, Show)
newtype Predicate = Predicate Vp deriving (Eq, Show)
data Adverbial = Tadvp Advp | Tpp Pp deriving (Eq, Show)
type Terminator = Maybe (W ())
type Advp = Connable AdvpC
data AdvpC = Advp (Pos () {-t7-}) Vp deriving (Eq, Show)
type Pp = Connable PpC
data PpC = Pp Prep Np deriving (Eq, Show)
type Prep = Connable PrepC
data PrepC = Prep (Pos () {-t6-}) Vp deriving (Eq, Show)
type Np = Connable NpC
data NpC = Focused (W Text {-mao-}) NpF | Unf NpF deriving (Eq, Show)
data NpF = ArgRel NpR Rel | Unr NpR deriving (Eq, Show)
data NpR = Npro (W Text) | Ndp Dp | Ncc Cc deriving (Eq, Show)
data Dp = Dp (W Determiner) (Maybe Vp) deriving (Eq, Show)
type Rel = Connable RelC
data RelC = Rel Statement Terminator deriving (Eq, Show) -- t3
data Cc = Cc Statement Terminator deriving (Eq, Show) -- t5
type Vp = Connable VpC
data VpC = Serial VpN VpC | Nonserial VpN deriving (Eq, Show)
data VpN -- nonserial verb phrase
    = Vname (W NameVerb) Name Terminator
    | Vshu (W ()) (Pos Text)
    | Voiv (W Text) Np Terminator
    | Vmo (W ()) Discourse Terminator
    | Vlu (W ()) Statement Terminator
    | Vverb (W Text)
    deriving (Eq, Show)
data Name = VerbName Vp | TermName Topic deriving (Eq, Show)

-- Parsers
type Parser t = Parsec [Pos Token] () t

-- | `many1`, but makes a `NonEmpty` list.
manyNE :: ParsecT s u m a -> ParsecT s u m (NonEmpty a)
manyNE p = (N.:|) <$> p <*> many p

-- | Parse a position-tagged token into `a`, if `f token` gives `Just a`.
tok :: (Token -> Maybe a) -> Parser (Pos a)
tok f = token show posPos (\(Pos x y t) -> Pos x y <$> f t)

-- | Parse a token if it equals the argument token.
tokEq :: Token -> Parser (Pos Token)
tokEq expected = tok (\x -> if x == expected then Just x else Nothing)

-- | Parse a token into () if it equals the argument token.
tokEq_ :: Token -> Parser (Pos ())
tokEq_ = fmap (()<$) . tokEq

pInterjection :: Parser (Pos Toned)
pInterjection = tok $ \t -> case t of Interjection x -> Just x; _ -> Nothing

pFreeMod :: Parser FreeMod
pFreeMod =
    (Fint <$> pInterjection)
    <|> (Fvoc <$> tokEq_ Hu <*> pNp)
    <|> (Finc <$> tokEq_ Ju <*> pSentence)
    <|> (Fpar <$> tokEq_ Kio <*> pDiscourse <*> pKi)

pW :: Parser (Pos a) -> Parser (W a)
pW p = W <$> p <*> many pFreeMod

pFocuser :: Parser (W Text)
pFocuser = pW $ tok $ \t -> case t of Focuser te -> Just te; _ -> Nothing

pDeterminer :: Parser (W Determiner)
pDeterminer = pW $ tok $ \t -> case t of Determiner d -> Just d; _ -> Nothing

pConnective :: Parser (W Connective)
pConnective = pW $ tok $ \t -> case t of Connective d -> Just d; _ -> Nothing

pComplementizerT3, pComplementizerT4, pComplementizerT5 :: Parser (W Complementizer)
pComplementizerT3 = pW $ tok $ \t -> case t of Complementizer c@(CT3 _) -> Just c; _ -> Nothing
pComplementizerT4 = pW $ tok $ \t -> case t of Complementizer c@(CT4 _) -> Just c; _ -> Nothing
pComplementizerT5 = pW $ tok $ \t -> case t of Complementizer c@(CT5 _) -> Just c; _ -> Nothing

pOiv :: Parser (W Text)
pOiv = pW $ tok $ \t -> case t of Oiv te -> Just te; T4jei -> Just "jei"; _ -> Nothing

pPronoun :: Parser (W Text)
pPronoun = pW $ tok $ \t -> case t of Pronoun te -> Just te; _ -> Nothing

pNameVerb :: Parser (W NameVerb)
pNameVerb = pW $ tok $ \t -> case t of NameVerb nv -> Just nv; _ -> Nothing

pShu, pMo, pLu :: Parser (W ())
pShu = pW $ tok $ \t -> case t of Shu -> Just (); _ -> Nothing
pMo = pW $ tok $ \t -> case t of Mo -> Just (); _ -> Nothing
pLu = pW $ tok $ \t -> case t of Lu -> Just (); _ -> Nothing

pTerminator :: Token -> Parser Terminator
pTerminator = optionMaybe . pW . tokEq_

pT6token, pT7token :: Parser (Pos ())
pT6token = tok $ \t -> case t of T6token -> Just (); _ -> Nothing
pT7token = tok $ \t -> case t of T7token -> Just (); _ -> Nothing

-- | Helper for parsing connectable constructs.
-- @'pConnable'' pNa pFirst pRest@ parses a @pFirst@ followed by many @pRest@s,
-- joined by TO RUs / RUs as expected by Toaq grammar.
-- If afterthought constructs require a __na__ before the RU, the @pNa@ argument
-- should be a parser for the __na__ token; if not, it should be @pure ()@ to
-- parse nothing
pConnable' :: Parser na -> Parser c -> Parser c -> Parser (Connable' na c)
pConnable' pNa pFirst pRest =
    (ConnTo <$> pTo <*> pConnective <*> pConnable' pNa pFirst pRest <*> pTo <*> pConnable' pNa pRest pRest)
    <|> pAfterthought
  where
    pAfterthought = do
        left <- pFirst
        right <- optionMaybe $ (,,) <$> pNa <*> pConnective <*> pConnable' pNa pRest pRest
        pure $ case right of
            Nothing -> Single left
            Just (na, ru, rest) -> Conn left na ru rest

-- Connectable constructs not requiring "na" in afterthought.
pConnable :: Parser c -> Parser c -> Parser (Connable' () c)
pConnable = pConnable' (pure ())

-- Connectable constructs requiring "na" in afterthought.
pConnableNa :: Parser c -> Parser c -> Parser (Connable' (W ()) c)
pConnableNa = pConnable' (pW $ tokEq_ Na)

-- Connectable constructs where the left and right terms are parsed the same way.
-- (Example: Gĩ ru hũı -- AdvP ru AdvP.)
-- (Counterexample: Gí ru hủı -- T2 on the left and T4 on the right.)
pConnableSame :: Parser c -> Parser (Connable' () c)
pConnableSame p = pConnable p p

pBi, pKi, pTo :: Parser (W ())
pBi = pW (tokEq_ Bi)
pKi = pW (tokEq_ Ki)
pTo = pW (tokEq_ To)
-- hu, ju, kio handled inside pW

pCy, pGa, pKy, pTeo :: Parser Terminator
pCy = pTerminator Cy
pGa = pTerminator Ga
pKy = pTerminator Ky
pTeo = pTerminator Teo

pIllocution :: Parser (W Toned)
pIllocution = pW $ tok $ \t -> case t of Illocution x -> Just x; _ -> Nothing

pSentenceConnector :: Parser (W Text)
pSentenceConnector = pW $ tok $ \t ->
    case t of
        SentenceConnector x -> Just x
        Connective c -> Just (T.toLower $ T.pack $ show c) -- hack
        _ -> Nothing

pVerb :: Parser (W Text)
pVerb = pW $ tok $ \t -> case t of Verb te -> Just te; _ -> Nothing

pRawWord :: Parser (Pos Text)
pRawWord = token show posPos (\(Pos p src _) -> Just (Pos p src src))

pName :: Parser Name
pName = (VerbName <$> pVp) <|> (TermName <$> pTopic)

pVpN :: Parser VpN
pVpN =
    (Vname <$> pNameVerb <*> pName <*> pGa)
    <|> (Vshu <$> pShu <*> pRawWord)
    <|> (Voiv <$> pOiv <*> pNp <*> pGa)
    <|> (Vmo <$> pMo <*> pDiscourse <*> pTeo)
    <|> (Vlu <$> pLu <*> pStatementT4 <*> pKy)
    <|> (Vverb <$> pVerb)

pVp :: Parser Vp
pVp = pConnable pVpC pVpC

pVpC :: Parser VpC
pVpC = do head <- pVpN; (Serial head <$> pVpC) <|> pure (Nonserial head)

pDp :: Parser Dp
pDp = do
    d <- pDeterminer
    vp <- if unW d == DT2 then Just <$> pVp else optionMaybe pVp
    pure $ Dp d vp

pNp :: Parser Np
pNp = pConnableSame pNpC

pNpC :: Parser NpC
pNpC = (Focused <$> pFocuser <*> pNpF) <|> (Unf <$> pNpF)

pNpF :: Parser NpF
pNpF = do head <- pNpR; (ArgRel head <$> pRel) <|> pure (Unr head)

pNpR :: Parser NpR
pNpR = (Npro <$> pPronoun) <|> (Ndp <$> pDp) <|> (Ncc <$> pCc)

pCc :: Parser Cc
pCc = Cc <$> pStatement (Just <$> pComplementizerT5) <*> pCy

pRel :: Parser Rel
pRel = pConnableSame (Rel <$> pStatement (Just <$> pComplementizerT3) <*> pCy)

pAdvp :: Parser Advp
pAdvp = pConnableSame (Advp <$> pT7token <*> pVp)

pPrep :: Parser Prep
pPrep = pConnableSame (Prep <$> pT6token <*> pVp)

pPp :: Parser Pp
pPp = pConnableSame (Pp <$> pPrep <*> pNp)

pTopic :: Parser Topic
pTopic = (Topicn <$> pNp) <|> (Topica <$> pAdverbial)

pAdverbial :: Parser Adverbial
pAdverbial = (Tadvp <$> pAdvp) <|> (Tpp <$> pPp)

pPredicate :: Parser Predicate
pPredicate = Predicate <$> pVp

pPredicationC :: Parser PredicationC
pPredicationC = Predication <$> pPredicate <*> many pAdverbial <*> many pNp <*> many pAdverbial

pPredication :: Parser Predication
pPredication = pConnableNa pPredicationC pPredicationC

pStatement :: Parser (Maybe (W Complementizer)) -> Parser Statement
pStatement pc = Statement <$> pc <*> optionMaybe pPrenex <*> pPredication

pStatementT4 :: Parser Statement
pStatementT4 = pStatement (optionMaybe pComplementizerT4)

pPrenex :: Parser Prenex
pPrenex = Prenex <$> manyNE pTopic <*> pBi

pSentence :: Parser Sentence
pSentence = Sentence <$> optionMaybe pSentenceConnector <*> pStatementT4 <*> optionMaybe pIllocution

pFragment :: Parser Fragment
pFragment = try (FrPrenex <$> pPrenex) <|> (FrTopic <$> pTopic)

pDiscourseItem :: Parser DiscourseItem
pDiscourseItem = (DiFree <$> pFreeMod) <|> try (DiSentence <$> pSentence) <|> (DiFragment <$> pFragment)

pDiscourse :: Parser Discourse
pDiscourse = Discourse <$> many pDiscourseItem

parseDiscourse :: [Pos Token] -> Either ParseError Discourse
parseDiscourse = parse (pDiscourse <* eof) "" . traceShowId
