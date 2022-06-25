module Parse where
import Lex
import Data.Functor (void)
import Data.List.NonEmpty qualified as N
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import Data.Text (Text)
import Text.Parsec as P

-- Most leaves of the parse tree are "W" (words with possible free modifiers before them).
data W t = WordF FreeMod (W t) | Word (Pos t) deriving (Eq, Functor, Show)
data FreeMod
    = Fint (Pos Toned)
    | Fvoc (Pos () {-hu-}) Np
    | Finc (Pos () {-ju-}) Sentence
    | Fpar (Pos () {-kio-}) Discourse (W () {-ki-})
    deriving (Eq, Show)

-- Generic type for constructs which can be connected like "X (na) ru Y", or "to ru X to Y", or occur alone.
data Connable' na c
    = Conn c na Connective (Connable c)
    | ConnTo (W () {-to-}) Connective (Connable c) (W () {-to-}) (Connable c)
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
data Statement = Statement (Maybe Prenex) Predication deriving (Eq, Show)
type Predication = ConnableNa PredicationC
data PredicationC = CompPredication (W Complementizer) Statement | SimplePredication PredicationS deriving (Eq, Show)
data PredicationS = Predication Predicate [Term] deriving (Eq, Show)
data Predicate = Predicate Vp deriving (Eq, Show)
data Term = Tnp Np | Tap Advp | Tpp Pp deriving (Eq, Show)
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
tok f = token show getPos (\(Pos x y t) -> Pos x y <$> f t)

tokEq :: Token -> Parser (Pos Token)
tokEq t = tok (\x -> if x == t then Just t else Nothing)

tokEq_ :: Token -> Parser (Pos ())
tokEq_ = fmap void . tokEq

pInterjection :: Parser (Pos Toned)
pInterjection = tok $ \t -> case t of Interjection x -> Just x; _ -> Nothing

pFreeMod :: Parser FreeMod
pFreeMod = Fint <$> pInterjection --  <|> todo

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

pBi = pW (tokEq_ Bi)
pCy = pTerminator Cy
pGa = pTerminator Ga
-- pHu = pW (tokEq_ Hu)  -- handled inside pW
-- pJu = pW (tokEq_ Ju)  -- handled inside pW
pKi = pW (tokEq_ Ki)
-- pKio = pW (tokEq_ Kio)  -- handled inside pW
pKy = pTerminator Ky
pTeo = pW (tokEq_ Teo)
pTo = pW (tokEq_ To)

pIllocution :: Parser (W Toned)
pIllocution = pW $ tok $ \t -> case t of Illocution x -> Just x; _ -> Nothing

pSentenceConnector :: Parser (W Text)
pSentenceConnector = pW $ tok $ \t -> case t of SentenceConnector x -> Just x; _ -> Nothing

pVerb :: Tone -> Parser (W Text)
pVerb tone = pW $ tok $ \t -> case t of Verb (te,to) | to == tone -> Just te; _ -> Nothing

pRawWord :: Parser (Pos Text)
pRawWord = token show getPos (\(Pos p src _) -> Just (Pos p src src))

pName :: Parser Name
pName = VerbName <$> pVp T4

pVpN :: Tone -> Parser VpN
pVpN t =
    (Vname <$> pNameVerb t <*> pName <*> pGa)
    <|> (Vshu <$> pShu t <*> pRawWord)
    -- <|> (Voiv <$> pOiv t <*> pNp <*> pGa)
    -- <|> (Vmo <$> pMo t <*> pDiscourse <*> pTeo)
    -- <|> (Vlu <$> pLu t <*> pStatement <*> pKy)
    <|> (Vverb <$> pVerb t)

pVp :: Tone -> Parser Vp
pVp t = Single <$> pVpC t -- TODO connectives

pVpC :: Tone -> Parser VpC
pVpC tone = do
    h <- pVpN tone
    (Serial h <$> pVpC T4) <|> pure (Nonserial h)

