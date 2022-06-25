module Parse where
import Lex
import Data.Functor (void)
import Data.List.NonEmpty qualified as N
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import Data.Text (Text)
import Text.Parsec as P

-- Most leaves of the parse tree are "W" (words with possible free modifiers before them).
data W t = WordF FreeMod (W t) | Word (Pos t) deriving (Eq, Functor)
instance Show t => Show (W t) where
    show (Word (Pos p s v)) = "\x1b[93m" ++ T.unpack s ++ "\x1b[0m"
    show (WordF _ w) = "\x1b[32m#\x1b[0m" ++ show w

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

tok :: (Token -> Maybe a) -> Parser (Pos a)
tok f = token show posPos (\(Pos x y t) -> Pos x y <$> f t)

tokEq :: Token -> Parser (Pos Token)
tokEq t = tok (\x -> if x == t then Just t else Nothing)

tokEq_ :: Token -> Parser (Pos ())
tokEq_ = fmap void . tokEq

pInterjection :: Parser (Pos Toned)
pInterjection = tok $ \t -> case t of Interjection x -> Just x; _ -> Nothing

pFreeMod :: Parser FreeMod
pFreeMod =
    try (Fint <$> pInterjection)
    <|> try (Fvoc <$> tokEq_ Hu <*> pNp)
    <|> try (Finc <$> tokEq_ Ju <*> pSentence)
    <|> (Fpar <$> tokEq_ Kio <*> pDiscourse <*> pKi)

pW :: Parser (Pos a) -> Parser (W a)
pW p = WordF <$> pFreeMod <*> pW p <|> Word <$> p

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

pConnable' :: Parser na -> Parser c -> Parser c -> Parser (Connable' na c)
pConnable' pNa p1 p2 =
    try (Conn <$> p1 <*> pNa <*> pConnective <*> pConnable' pNa p2 p2)
    <|> try (ConnTo <$> pTo <*> pConnective <*> pConnable' pNa p1 p2 <*> pTo <*> pConnable' pNa p2 p2)
    <|> (Single <$> p1)

pConnable = pConnable' (pure ())
pConnableNa = pConnable' (pW $ tokEq_ Na)
pConnableSame p = pConnable p p

pBi = pW (tokEq_ Bi)
pCy = pTerminator Cy
pGa = pTerminator Ga
-- pHu = pW (tokEq_ Hu)  -- handled inside pW
-- pJu = pW (tokEq_ Ju)  -- handled inside pW
pKi = pW (tokEq_ Ki)
-- pKio = pW (tokEq_ Kio)  -- handled inside pW
pKy = pTerminator Ky
pTeo = pTerminator Teo
pTo = pW (tokEq_ To)

pIllocution :: Parser (W Toned)
pIllocution = pW $ tok $ \t -> case t of Illocution x -> Just x; _ -> Nothing

pSentenceConnector :: Parser (W Text)
pSentenceConnector = pW $ tok $ \t -> case t of SentenceConnector x -> Just x; _ -> Nothing

pVerb :: Tone -> Parser (W Text)
pVerb tone = pW $ tok $ \t -> case t of Verb (te,to) | to == tone -> Just te; _ -> Nothing

pRawWord :: Parser (Pos Text)
pRawWord = token show posPos (\(Pos p src _) -> Just (Pos p src src))

pName :: Parser Name
pName = try (VerbName <$> pVp T4) <|> (TermName <$> pTerm)

pVpN :: Tone -> Parser VpN
pVpN t =
    try (Vname <$> pNameVerb t <*> pName <*> pGa)
    <|> try (Vshu <$> pShu t <*> pRawWord)
    <|> try (Voiv <$> pOiv t <*> pNp <*> pGa)
    <|> try (Vmo <$> pMo t <*> pDiscourse <*> pTeo)
    <|> try (Vlu <$> pLu t <*> pStatement <*> pKy)
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
pNpC = try (Focused <$> pFocuser <*> pNpF) <|> (Unf <$> pNpF)

pNpF :: Parser NpF
pNpF = do head <- pNpR; try (ArgRel head <$> pRel) <|> pure (Unr head)

pNpR :: Parser NpR
pNpR = try (Bound <$> pVp T2) <|> try (Ndp <$> pDp) <|> (Ncc <$> pCc)

-- I got lazy with the type annotations here
pCc = Cc <$> pPredication T5 <*> pKy
pRel = pConnableSame (Rel <$> pPredication T3 <*> pKy)
manyNE p = (N.:|) <$> p <*> many p
pDiscourse = Discourse <$> many pDiscourseItem
pDiscourseItem = try (DiSentence <$> pSentence) <|> try (DiFragment <$> pFragment) <|> (DiFree <$> pFreeMod)
pSentence = Sentence <$> optionMaybe (try pSentenceConnector) <*> pStatement <*> optionMaybe (try pIllocution)
pFragment = try (FrPrenex <$> pPrenex) <|> (FrTerms <$> manyNE pTerm)
pPrenex = Prenex <$> manyNE pTerm <*> pBi
pStatement = Statement <$> optionMaybe (try pPrenex) <*> pPredicationsRubi
pPredicationsRubi = do h <- pPredication T4; try (Rubi h <$> pConnective <*> pBi <*> pPredicationsRubi) <|> pure (NonRubi h)
pStatement_nocomp = Statement <$> optionMaybe (try pPrenex) <*> (NonRubi . Single . SimplePredication <$> pPredicationS T4)
pPredication tone = pConnableNa (pPredicationC tone) (pPredicationC T4)
pPredicationC tone = try (CompPredication <$> pComplementizer tone <*> pStatement_nocomp) <|> (SimplePredication <$> pPredicationS tone)
pPredicationS tone = Predication <$> pPredicate tone <*> many pTerm
pPredicate tone = Predicate <$> pVp tone
pAdvp = pConnableSame (Advp <$> pVp T7)
pPrep = pConnableSame (Prep <$> pVp T6)
pPp = pConnableSame (Pp <$> pPrep <*> pNp)
pTerm = try (Tnp <$> pNp) <|> try (Tadvp <$> pAdvp) <|> (Tpp <$> pPp)
