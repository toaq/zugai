module Parse where
import Lex
import Data.List.NonEmpty qualified as N
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import Data.Text (Text)
import Text.Parsec as P

-- Most leaves of the parse tree are "W" (words with possible free modifiers after them).
data W t = W (Pos t) [FreeMod] deriving (Eq, Functor)
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
data Discourse = Discourse [DiscourseItem] deriving (Eq, Show)
data DiscourseItem = DiSentence Sentence | DiFragment Fragment | DiFree FreeMod deriving (Eq, Show)
data Sentence = Sentence (Maybe (W Text {-je-})) Statement (Maybe (W Toned {-da-})) deriving (Eq, Show)
data Fragment = FrPrenex Prenex | FrTerms (NonEmpty Term) deriving (Eq, Show)
data Prenex = Prenex (NonEmpty Term) (W () {-bi-}) deriving (Eq, Show)
data Statement = Statement (Maybe Prenex) PredicationsRubi deriving (Eq, Show)
data PredicationsRubi = Rubi Predication (W Connective) (W () {-bi-}) PredicationsRubi | NonRubi Predication deriving (Eq, Show)
type Predication = ConnableNa PredicationC
data PredicationC = CompPredication (W Complementizer) Statement | SimplePredication PredicationS deriving (Eq, Show)
data PredicationS = Predication Predicate [Term] deriving (Eq, Show)
data Predicate = Predicate Vp deriving (Eq, Show)
data Term = Tnp Np | Tadvp Advp | Tpp Pp deriving (Eq, Show)
type Terminator = Maybe (W ())
type Advp = Connable AdvpC
data AdvpC = Advp Vp deriving (Eq, Show)
type Pp = Connable PpC
data PpC = Pp Prep Np deriving (Eq, Show)
type Prep = Connable PrepC
data PrepC = Prep Vp deriving (Eq, Show)
type Np = Connable NpC
data NpC = Focused (W Text {-mao-}) NpF | Unf NpF deriving (Eq, Show)
data NpF = ArgRel NpR Rel | Unr NpR deriving (Eq, Show)
data NpR = Bound Vp | Ndp Dp | Ncc Cc deriving (Eq, Show)
data Dp = Dp (W Determiner) (Maybe Vp) deriving (Eq, Show)
type Rel = Connable RelC
data RelC = Rel Predication Terminator deriving (Eq, Show) -- t3
data Cc = Cc Predication Terminator deriving (Eq, Show) -- t5
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
data Name = VerbName Vp | TermName Term deriving (Eq, Show)

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

pComplementizer :: Tone -> Parser (W Complementizer)
pComplementizer tone = pW $ tok $ \t -> case t of Complementizer c to | to == tone -> Just c; _ -> Nothing

pOiv :: Tone -> Parser (W Text)
pOiv tone = pW $ tok $ \t -> case t of Oiv (te,to) | to == tone -> Just te; _ -> Nothing

pNameVerb :: Tone -> Parser (W NameVerb)
pNameVerb tone = pW $ tok $ \t -> case t of NameVerb nv to | to == tone -> Just nv; _ -> Nothing

pShu, pMo, pLu :: Tone -> Parser (W ())
pShu tone = pW $ tok $ \t -> case t of Shu to | to == tone -> Just (); _ -> Nothing
pMo tone = pW $ tok $ \t -> case t of Mo to | to == tone -> Just (); _ -> Nothing
pLu tone = pW $ tok $ \t -> case t of Lu to | to == tone -> Just (); _ -> Nothing

pTerminator :: Token -> Parser Terminator
pTerminator = optionMaybe . pW . tokEq_

-- Helper for parsing connectable constructs.
pConnable' :: Parser na -> Parser c -> Parser c -> Parser (Connable' na c)
pConnable' pNa p1 p2 =
    try (Conn <$> p1 <*> pNa <*> pConnective <*> pConnable' pNa p2 p2)
    <|> try (ConnTo <$> pTo <*> pConnective <*> pConnable' pNa p1 p2 <*> pTo <*> pConnable' pNa p2 p2)
    <|> (Single <$> p1)

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

pVerb :: Tone -> Parser (W Text)
pVerb tone = pW $ tok $ \t -> case t of Verb (te,to) | to == tone -> Just te; _ -> Nothing

pRawWord :: Parser (Pos Text)
pRawWord = token show posPos (\(Pos p src _) -> Just (Pos p src src))

pName :: Parser Name
pName = (VerbName <$> pVp T4) <|> (TermName <$> pTerm)

pVpN :: Tone -> Parser VpN
pVpN t =
    (Vname <$> pNameVerb t <*> pName <*> pGa)
    <|> (Vshu <$> pShu t <*> pRawWord)
    <|> (Voiv <$> pOiv t <*> pNp <*> pGa)
    <|> (Vmo <$> pMo t <*> pDiscourse <*> pTeo)
    <|> (Vlu <$> pLu t <*> pStatement <*> pKy)
    <|> (Vverb <$> pVerb t)

pVp :: Tone -> Parser Vp
pVp tone = pConnable (pVpC tone) (pVpC T4)

pVpC :: Tone -> Parser VpC
pVpC tone = do head <- pVpN tone; (Serial head <$> pVpC T4) <|> pure (Nonserial head)

pDp :: Parser Dp
pDp = Dp <$> pDeterminer <*> optionMaybe (pVp T4)

pNp :: Parser Np
pNp = pConnableSame pNpC

pNpC :: Parser NpC
pNpC = (Focused <$> pFocuser <*> pNpF) <|> (Unf <$> pNpF)

pNpF :: Parser NpF
pNpF = do head <- pNpR; try (ArgRel head <$> pRel) <|> pure (Unr head)

pNpR :: Parser NpR
pNpR = (Bound <$> pVp T2) <|> (Ndp <$> pDp) <|> (Ncc <$> pCc)

pCc :: Parser Cc
pCc = Cc <$> pPredication T5 <*> pCy

pRel :: Parser Rel
pRel = pConnableSame (Rel <$> pPredication T3 <*> pCy)

pAdvp :: Parser Advp
pAdvp = pConnableSame (Advp <$> pVp T7)

pPrep :: Parser Prep
pPrep = pConnableSame (Prep <$> pVp T6)

pPp :: Parser Pp
pPp = pConnableSame (Pp <$> pPrep <*> pNp)

pTerm :: Parser Term
pTerm = (Tnp <$> pNp) <|> (Tadvp <$> pAdvp) <|> (Tpp <$> pPp)

pPredicate :: Tone -> Parser Predicate
pPredicate tone = Predicate <$> pVp tone

pPredicationS :: Tone -> Parser PredicationS
pPredicationS tone = Predication <$> pPredicate tone <*> many pTerm

pPredicationC :: Tone -> Parser PredicationC
pPredicationC tone = (CompPredication <$> pComplementizer tone <*> pStatement_nocomp) <|> (SimplePredication <$> pPredicationS tone)

pPredication :: Tone -> Parser Predication
pPredication tone = pConnableNa (pPredicationC tone) (pPredicationC T4)

pStatement :: Parser Statement
pStatement = Statement <$> optionMaybe (try pPrenex) <*> pPredicationsRubi

pPredicationsRubi :: Parser PredicationsRubi
pPredicationsRubi = do h <- pPredication T4; try (Rubi h <$> pConnective <*> pBi <*> pPredicationsRubi) <|> pure (NonRubi h)

pStatement_nocomp :: Parser Statement
pStatement_nocomp = Statement <$> optionMaybe (try pPrenex) <*> (NonRubi . Single . SimplePredication <$> pPredicationS T4)

pPrenex :: Parser Prenex
pPrenex = Prenex <$> manyNE pTerm <*> pBi

pSentence :: Parser Sentence
pSentence = Sentence <$> optionMaybe pSentenceConnector <*> pStatement <*> optionMaybe pIllocution

pFragment :: Parser Fragment
pFragment = try (FrPrenex <$> pPrenex) <|> (FrTerms <$> manyNE pTerm)

pDiscourseItem :: Parser DiscourseItem
pDiscourseItem = (DiFree <$> pFreeMod) <|> try (DiSentence <$> pSentence) <|> (DiFragment <$> pFragment)

pDiscourse :: Parser Discourse
pDiscourse = Discourse <$> many pDiscourseItem

parseDiscourse :: Text -> Either ParseError Discourse
parseDiscourse text = lexer text >>= parse (pDiscourse <* eof) ""
