{-# LANGUAGE MultiParamTypeClasses #-}
module Interpret where

import Control.Monad.Trans.Cont
import Control.Monad.Identity
import Control.Monad.State
import Data.Char
import Data.Foldable
import Data.List
import Data.Map qualified as M
import Data.Map (Map)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text (Text)
import Debug.Trace

import Dictionary
import TextUtils
import ToName
import Lex
import Parse
import Scope

type VarName = Text -- like "P" or "B1".
-- Maybe De Bruijn stuff might be easier to deal with,
-- but I want to assign "good" names based on verbs in sentence,
-- like "sa fu" becoming F.

data InterpretState =
    InterpretState
        { usedVars :: [VarName]  -- for clarity, makeFreeVar should avoid even variables that have gone out of scope
        , scopes :: [Scope Tm]  -- cons = push, tail = pop
        -- maybe move popped scopes somewhere so we can interpret quasi-donkey-anaphora?
        , stDictionary :: Dictionary
        } deriving (Eq, Show)

-- terms
data Tm
    = Var VarName
    | Fus Tm Tm -- X roi Y
    | Quo Text -- quoted text
    | Ccl Formula -- not very "neo-Davidsonian" of me
    deriving (Eq, Show)

data Formula
    = Prd Text [Tm] -- choq(X,Y,Z)
    | Tru -- tautology
    | Equ Tm Tm
    | Neg Formula
    | Con Connective Formula Formula
    | Qua Determiner VarName Formula Formula -- ∃[x: P(x)] Q(x)
    | Let VarName Formula Formula -- let p ↔ F(x,y) in G(p,z)
    deriving (Eq, Show)

-- (p -> Interpret' r a): interpret p into an a-value, within a continuation that eventually results in r.
-- reset :: Cont r r -> Cont r' r
--
-- shift :: ((a -> r) -> Cont r r) -> Cont r a
-- So shift (\k -> ...) can make a "term-in-formula" continuation (Cont r a)
-- out of a function that uses k :: a -> r to specify a "formula" continuation (Cont r r).
-- Wow, I still don't understand it at all! See ContinuationLab.hs

type Interpret' r = ContT r (State InterpretState)
type Interpret = Interpret' Formula

instance HasScopes Tm (Interpret' r) where
    getScopes = gets scopes
    setScopes s = modify (\i -> i { scopes = s })

showCon :: Connective -> Text
showCon Ra = "∨"
showCon Ru = "∧"
showCon Ro = "⊻"
showCon Ri = "?"
showCon Roi = "INVALID ROI" -- should have become Fus x y

showQua :: Determiner -> Text
showQua Sa = "∃"
showQua Tuq = "Λ"
showQua Tu = "∀"
showQua Ke = "ι"
showQua Ja = "λ"
showQua (XShi q) = showQua q <> "¹"
showQua q = T.toLower (T.pack $ show q) <> " "

showTm :: Tm -> Text
showTm (Var v) = v
showTm (Fus x y) = "⌊" <> showTm x <> "," <> showTm y <> "⌉"
showTm (Quo t) = "«" <> t <> "»"
showTm (Ccl f) = "{" <> showFormula f <> "}"

showFormula :: Formula -> Text
showFormula (Prd p ts) = p <> "(" <> T.intercalate "," (showTm <$> ts) <> ")"
showFormula Tru = "⊤"
showFormula (Equ x y) = showTm x <> " = " <> showTm y
showFormula (Neg f) = "¬" <> showFormula f
showFormula (Con c f1 f2) = "(" <> T.unwords [showFormula f1, showCon c, showFormula f2] <> ")"
showFormula (Qua det var Tru f) = showQua det <> var <> ": " <> showFormula f
showFormula (Qua det var res f) = "[" <> showQua det <> var <> ": " <> showFormula res <> "] " <> showFormula f
showFormula (Let var f g) = "let " <> var <> " ↔ " <> showFormula f <> " in " <> showFormula g

makeFreeVar :: Maybe Text -> Interpret' r Text
makeFreeVar verb = do
    let letters = case verb of
                    Just text | Just (c, _) <- T.uncons text -> [toUpper c]
                    Nothing -> "ABCDEFGHJKLMNPQRSTUVWXYZ"
    let candidates = T.pack <$> [h:t | t <- "" : map show [1..], h <- letters]
    used <- gets usedVars
    let free = head (candidates \\ used)
    modify (\st -> st { usedVars = free : usedVars st })
    pure free

bareSrc :: W t -> Text
bareSrc (W (Pos _ src _) _) = bareToaq src

vpToName :: Vp -> Text
vpToName = toName

interpretConn :: Show a => (c -> Interpret a) -> Connable' na c -> Interpret a
interpretConn f (Single c) = f c
interpretConn f (Conn x na conn y) =
    shiftT $ \k -> do
        fx <- resetT $ f x >>= lift . k
        fy <- resetT $ interpretConn f y >>= lift . k
        pure $ Con (unW conn) fx fy
        -- lift $ Con (unW conn) <$> k t1 <*> k t2
interpretConn f (ConnTo to conn x to' y) =
    shiftT $ \k -> do
        fx <- resetT $ interpretConn f x >>= lift . k
        fy <- resetT $ interpretConn f y >>= lift . k
        pure $ Con (unW conn) fx fy

interpretDiscourse :: Discourse -> Interpret' [Formula] [Formula]
interpretDiscourse (Discourse dis) = do
    fss <- mapM interpretDiscourseItem dis
    pure (concat fss)

interpretDiscourseItem :: DiscourseItem -> Interpret' [Formula] [Formula]
interpretDiscourseItem (DiSentence se) = (:[]) <$> resetT (interpretSentence se)
interpretDiscourseItem _ = pure []

interpretSentence :: Sentence -> Interpret Formula
interpretSentence (Sentence je stmt da) = do
    pushScope
    f <- interpretStatement stmt
    popScope
    pure f

interpretStatement :: Statement -> Interpret Formula
interpretStatement (Statement Nothing rubis) = resetT (interpretRubis rubis)
interpretStatement (Statement (Just (Prenex ts bi)) rubis) =
    resetT $ do
        incrementArgsSeen -- hack: treat topics as >0
        mapM interpretTerm $ toList ts -- is it ok to ignore topics?
        interpretRubis rubis

interpretRubis :: PredicationsRubi -> Interpret Formula
interpretRubis (NonRubi p) = interpretPredication p
interpretRubis (Rubi p1 ru bi p2) = do
    f1 <- interpretPredication p1
    f2 <- interpretRubis p2
    pure $ Con (unW ru) f1 f2

interpretRel :: Rel -> Interpret Formula
interpretRel (Single c) = interpretRelC c
interpretRel (Conn x na conn y) = Con (unW conn) <$> interpretRelC x <*> interpretRel y
interpretRel (ConnTo to conn x to' y) = Con (unW conn) <$> interpretRel x <*> interpretRel y

interpretRelC :: RelC -> Interpret Formula
interpretRelC (Rel pred cy) = interpretPredication pred

interpretPredication :: Predication -> Interpret Formula
interpretPredication (Single c) = interpretPredicationC c
interpretPredication (Conn x na conn y) = Con (unW conn) <$> interpretPredicationC x <*> interpretPredication y
interpretPredication (ConnTo to conn x to' y) = Con (unW conn) <$> interpretPredication x <*> interpretPredication y

interpretPredicationC :: PredicationC -> Interpret Formula
interpretPredicationC (SimplePredication p) = interpretPredicationS p
interpretPredicationC (CompPredication comp stmt) = interpretStatement stmt -- not sure how to handle ma/tio

interpretPredicationS :: PredicationS -> Interpret Formula
interpretPredicationS (Predication (Predicate vp) terms) =
    resetT $ do
        f <- interpretVp vp
        resetArgsSeen
        xs <- mapM interpretTerm terms
        f $/ concat xs

interpretTerm :: Term -> Interpret [Tm]
interpretTerm (Tnp np) = do t <- interpretNp np; incrementArgsSeen; pure [t]
interpretTerm (Tadvp advp) = interpretConn interpretAdvpC advp
interpretTerm (Tpp pp) = interpretConn interpretPpC pp
interpretTerm (Termset to ru ts1 to' ts2) = do
    shiftT $ \k -> do
        xs1 <- concat <$> mapM interpretTerm ts1
        xs2 <- concat <$> mapM interpretTerm ts2
        lift $ Con (unW ru) <$> k xs1 <*> k xs2

interpretNp :: Np -> Interpret Tm
interpretNp = interpretConn interpretNpC

interpretNpC :: NpC -> Interpret Tm
interpretNpC (Unf npf) = interpretNpF npf
interpretNpC (Focused mao npf) =
    shiftT $ \k -> do
        t <- interpretNpF npf
        v <- makeFreeVar Nothing
        lift $ do
            formula <- k (Var v)
            pure $ Prd (bareSrc mao <> "jeo") [t, Ccl (Qua Ja v Tru formula)]

interpretNpF :: NpF -> Interpret Tm
interpretNpF (Unr npr) = interpretNpR npr
interpretNpF (ArgRel (Bound vp) rel) = error "todo RelP on bound var?"
interpretNpF (ArgRel (Ncc cc) rel) = error "todo: RelP on content clause"
interpretNpF (ArgRel (Ndp (Dp det vp)) rel) =
    let transform verb =
            TmFun 1 1 "a" $ \[t] -> do
                tf <- case verb of Just v -> do x <- v$/[t]; pure $ \y -> Con Ru x y
                                   Nothing -> pure id
                pushScope
                bind "hoa" t -- todo autohoa!?
                f_rel <- interpretConn interpretRelC rel
                popScope
                pure $ tf f_rel
    in bindVpWithTransform (Just . transform) (unW det) vp

isHighVerb :: Vp -> Bool
isHighVerb vp = head (T.words (vpToName vp)) `elem` T.words "bu nai pu jia chufaq lui za hoai hai he hiq she ao dai ea le di duai"

interpretAdvpC :: AdvpC -> Interpret [Tm]
interpretAdvpC (Advp vp) = do
    if isHighVerb vp then
        shiftT $ \k -> do
            f <- interpretVp vp
            formula <- lift (k [])
            f $/ [Ccl $ formula]
            -- f $/ [Ccl formula]
    else
        shiftT $ \k -> do
            f <- interpretVp vp
            formula <- lift (k [])
            var <- makeFreeVar Nothing
            adverbial <- f $/ [Var var]
            pure (Let var formula (Con Ru (Prd "faq" [Var var]) adverbial))

interpretPpC :: PpC -> Interpret [Tm]
interpretPpC (Pp prep np) = pure []

bindVpWithTransform :: (Maybe VerbFun -> Maybe VerbFun) -> Determiner -> Maybe Vp -> Interpret Tm
bindVpWithTransform verbTransform det Nothing =
    shiftT $ \k -> do
        v <- makeFreeVar Nothing
        restriction <-
            case verbTransform Nothing of
                Just f' -> f' $/ [Var v]
                Nothing -> pure Tru
        lift $ Qua det v restriction <$> k (Var v)
bindVpWithTransform verbTransform det (Just vp) =
    shiftT $ \k -> do
        let name = vpToName vp
        v <- makeFreeVar (Just name)
        -- bind vars in the interpreter state:
        bind name (Var v)
        Scope argsSeen _ <- head <$> gets scopes
        di <- gets stDictionary
        let anaphora = if argsSeen == 0 then Just "aq" else lookupPronoun di name
        case anaphora of
            Just prn -> bind prn (Var v)
            Nothing -> pure ()
        f <- interpretVp vp
        restriction <-
            case verbTransform (Just f) of
                Just f' -> f' $/ [Var v]
                Nothing -> pure Tru
        lift $ Qua det v restriction <$> k (Var v)

bindVp :: Determiner -> Maybe Vp -> Interpret Tm
bindVp = bindVpWithTransform id

interpretNpR :: NpR -> Interpret Tm
interpretNpR (Bound vp) = do
    let name = vpToName vp
    ss <- gets scopes
    case msum $ map (M.lookup name . bindings) ss of
        Just tm -> pure tm
        Nothing -> bindVp Ke (Just vp)
interpretNpR (Ndp (Dp det vp)) = bindVp (unW det) vp
interpretNpR (Ncc (Cc predication cy)) = do
    f <- resetT $ do
        pushScope
        f' <- interpretPredication predication
        popScope
        pure f'
    bind "rou" (Ccl f) -- probably better to bind it to a var...
    pure (Ccl f)

-- A function [Tm] -> a, tagged with min and max arity and a serial frame.
data TmFun a = TmFun Int Int Text ([Tm] -> a)

applyTmFun :: TmFun a -> [Tm] -> Interpret a
applyTmFun (TmFun low high frame f) ts = do
    -- Pad with "sa rai" args until there are at least "low" terms.
    exs <- replicateM (max (low - length ts) 0) (bindVp Sa Nothing)
    -- Ignore all terms past "high". Maybe error?
    pure $ f $ take high $ ts ++ exs

($/) :: TmFun (Interpret a) -> [Tm] -> Interpret a
f $/ ts = join $ applyTmFun f ts

type VerbFun = TmFun (Interpret Formula)

interpretVp :: Vp -> Interpret VerbFun
interpretVp (Single c) = interpretVpC c
interpretVp (Conn x na ru y) = do
    v1 <- interpretVpC x
    v2 <- interpretVp y
    pure $ TmFun 0 999 "" $ \ts -> Con (unW ru) <$> (v1$/ts) <*> (v2$/ts)
interpretVp (ConnTo to ru x to' y) = do
    v1 <- interpretVp x
    v2 <- interpretVp y
    pure $ TmFun 0 999 "" $ \ts -> Con (unW ru) <$> (v1$/ts) <*> (v2$/ts)

serialize :: VerbFun -> VerbFun -> VerbFun
serialize v1@(TmFun l1 h1 frame1 f1) v2@(TmFun l2 h2 frame2 f2) =
    case frame1 of
        -- TODO: generalize
        "a" ->
            TmFun (max 1 l2) h2 frame2 $ \ts -> do
                p2 <- v2 $/ ts
                p1 <- v1 $/ [head ts]
                pure $ Con Ru p2 p1 -- not quite right, handle attributive adj
        "0" ->
            TmFun l2 h2 frame2 $ \ts -> do
                p2 <- v2 $/ ts
                p1 <- v1 $/ [Ccl p2]
                pure $ p1
        "c 0" ->
            TmFun (max 1 l2) h2 frame2 $ \(t:ts) -> do
                p2 <- v2 $/ ts
                p1 <- v1 $/ [t, Ccl p2]
                pure $ p1
        "c c 0" ->
            TmFun (max 2 l2) h2 frame2 $ \(t:t':ts) -> do
                p2 <- v2 $/ ts
                p1 <- v1 $/ [t, t', Ccl p2]
                pure $ p1
        "c 1" ->
            TmFun (max 1 l2) h2 frame2 $ \(t:ts) -> do
                v <- makeFreeVar Nothing
                p2 <- v2 $/ (Var v:ts)
                p1 <- v1 $/ [t, Ccl (Qua Ja v Tru p2)]
                pure $ p1
        "c c 1" ->
            TmFun (max 2 l2) h2 frame2 $ \(t:t':ts) -> do
                v <- makeFreeVar Nothing
                p2 <- v2 $/ (Var v:ts)
                p1 <- v1 $/ [t, t', Ccl (Qua Ja v Tru p2)]
                pure $ p1
        "c 2" ->
            TmFun (max 1 l2) h2 frame2 $ \(t:ts) -> do
                v <- makeFreeVar Nothing
                v' <- makeFreeVar Nothing
                p2 <- v2 $/ (Var v:Var v':ts)
                p1 <- v1 $/ [t, Ccl (Qua Ja v Tru (Qua Ja v' Tru p2))]
                pure $ p1
        "c c 2" ->
            TmFun (max 2 l2) h2 frame2 $ \(t:t':ts) -> do
                v <- makeFreeVar Nothing
                v' <- makeFreeVar Nothing
                p2 <- v2 $/ (Var v:Var v':ts)
                p1 <- v1 $/ [t, t', Ccl (Qua Ja v Tru (Qua Ja v' Tru p2))]
                pure $ p1


interpretVpC :: VpC -> Interpret VerbFun
interpretVpC (Nonserial vpn) = interpretVpN vpn
interpretVpC (Serial vpn vpc) = do
    v1 <- interpretVpN vpn
    v2 <- interpretVpC vpc
    pure (serialize v1 v2)

interpretVpN :: VpN -> Interpret VerbFun
interpretVpN (Vname nv v ga)          = pure $ TmFun 1 1 "a" $ \[t] -> pure $ Prd "chua" [Quo $ toName v, t]
interpretVpN (Vshu shu (Pos _ src _)) = pure $ TmFun 1 1 "a" $ \[t] -> pure $ Equ t (Quo src)
interpretVpN (Vmo mo txt teo)         = pure $ TmFun 1 1 "a" $ \[t] -> pure $ Equ t (Quo (T.pack $ show txt))
interpretVpN (Voiv oiv np ga) = do
    t <- interpretNp np
    pure $ TmFun 1 1 "a" $ \[u] -> pure $ Prd (bareSrc oiv <> "ga") [t,u]
interpretVpN (Vlu lu stmt ky) = do
    pure $ TmFun 1 1 "a" $ \[t] -> do
        pushScope
        bind "hoa" t
        f <- interpretStatement stmt
        popScope
        pure f
interpretVpN (Vverb v) = do
    dict <- gets stDictionary
    let frame = maybe "a" id $ lookupFrame dict (bareSrc v)
    pure $ TmFun 0 999 frame $ \ts -> pure $ Prd (bareSrc v) ts

interpret :: Dictionary -> Discourse -> [Formula]
interpret dict discourse =
    evalState
        (evalContT (interpretDiscourse discourse))
        (InterpretState [] [emptyScope] dict)

lpi :: Dictionary -> Text -> [Formula]
lpi dict text =
    let
        Right tokens = lexToaq text
        Right discourse = parseDiscourse tokens
    in
        interpret dict discourse

lpis :: Text -> IO ()
lpis text = do
    dict <- readDictionary
    mapM_ (T.putStrLn . showFormula) $ lpi dict text
