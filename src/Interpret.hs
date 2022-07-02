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

import Dictionary
import TextUtils
import ToName
import Lex
import Parse

type VarName = Text -- like "P" or "B1".
-- Maybe De Bruijn stuff might be easier to deal with,
-- but I want to assign "good" names based on verbs in sentence,
-- like "sa fu" becoming F.

type VarRef = Text -- like "de poq" or "ta": VP-turned-to-text or pronoun word that refers to a variable
type Scope = Map VarRef Tm  -- after "sa dẻ pỏq", generate var "D" and map "de poq" + "ta" to "D" here.

data InterpretState =
    InterpretState
        { usedVars :: [VarName]  -- for clarity, makeFreeVar should avoid even variables that have gone out of scope
        , scopes :: [Scope]  -- cons = push, tail = pop
        -- maybe move popped scopes somewhere so we can interpret quasi-donkey-anaphora?
        , stDictionary :: Dictionary
        }
        deriving (Eq, Show)

-- terms
data Tm
    = Var VarName
    | Fus Tm Tm -- X roi Y
    | Quo Text -- quoted text
    | Evt Formula -- not very "neo-Davidsonian" of me
    deriving (Eq, Show)

data Formula
    = Prd Text [Tm] -- choq(X,Y,Z)
    | Tru -- tautology
    | Equ Tm Tm
    | Neg Formula
    | Con Connective Formula Formula
    | Qua Determiner VarName Formula Formula -- ∃[x: P(x)] Q(x)
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
showQua q = T.pack $ show q

showTm :: Tm -> Text
showTm (Var v) = v
showTm (Fus x y) = "⌊" <> showTm x <> "," <> showTm y <> "⌉"
showTm (Quo t) = "«" <> t <> "»"
showTm (Evt f) = "{" <> showFormula f <> "}"

showFormula :: Formula -> Text
showFormula (Prd p ts) = p <> "(" <> T.intercalate "," (showTm <$> ts) <> ")"
showFormula Tru = "⊤"
showFormula (Equ x y) = showTm x <> " = " <> showTm y
showFormula (Neg f) = "¬" <> showFormula f
showFormula (Con c f1 f2) = "(" <> T.unwords [showFormula f1, showCon c, showFormula f2] <> ")"
showFormula (Qua det var Tru f) = showQua det <> var <> ": " <> showFormula f
showFormula (Qua det var res f) = "[" <> showQua det <> var <> ": " <> showFormula res <> "] " <> showFormula f

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

pushScope :: Interpret' r ()
pushScope = modify (\st -> st { scopes = M.empty : scopes st })

popScope :: Interpret' r Scope
popScope = do
    ss <- gets scopes
    modify (\st -> st { scopes = tail ss })
    pure (head ss)

modifyTop :: (Scope -> Scope) -> Interpret' r ()
modifyTop f = do
    ss <- gets scopes
    modify (\st -> st { scopes = f (head ss) : tail ss })

bind :: VarRef -> Tm -> Interpret' r ()
bind var term = modifyTop (M.insert var term)

bareSrc :: W t -> Text
bareSrc (W (Pos _ src _) _) = bareToaq src

vpToName :: Vp -> Text
vpToName = toName

interpretConn :: (c -> Interpret a) -> Connable' na c -> Interpret a
interpretConn f (Single c) = f c
interpretConn f (Conn x na conn y) =
    shiftT $ \k -> do
        t1 <- f x
        t2 <- interpretConn f y
        lift $ Con (unW conn) <$> k t1 <*> k t2
interpretConn f (ConnTo to conn x to' y) =
    shiftT $ \k -> do
        t1 <- interpretConn f x
        t2 <- interpretConn f y
        lift $ Con (unW conn) <$> k t1 <*> k t2

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
        mapM interpretTerm $ toList ts -- is it ok to ignore topics?
        interpretRubis rubis

interpretRubis :: PredicationsRubi -> Interpret Formula
interpretRubis (NonRubi p) = interpretPredication p
interpretRubis (Rubi p1 ru bi p2) = do
    f1 <- interpretPredication p1
    f2 <- interpretRubis p2
    pure $ Con (unW ru) f1 f2

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
        ts <- mapM interpretTerm terms
        f $/ concat ts

interpretTerm :: Term -> Interpret [Tm]
interpretTerm (Tnp np) = (:[]) <$> interpretNp np
interpretTerm _ = error "term"

interpretNp :: Np -> Interpret Tm
interpretNp = interpretConn interpretNpC

interpretNpC :: NpC -> Interpret Tm
interpretNpC (Unf npf) = interpretNpF npf
interpretNpC (Focused mao npf) = error "todo focus"

interpretNpF :: NpF -> Interpret Tm
interpretNpF (Unr npr) = interpretNpR npr
interpretNpF (ArgRel arg rel) = error "todo relp"

bindVp :: Determiner -> Maybe Vp -> Interpret Tm
bindVp det Nothing =
    shiftT $ \k -> do
        v <- makeFreeVar Nothing
        lift $ Qua det v Tru <$> k (Var v)
bindVp det (Just vp) =
    shiftT $ \k -> do
        let name = vpToName vp
        v <- makeFreeVar (Just name)
        -- bind vars in the interpreter state:
        bind name (Var v)
        di <- gets stDictionary
        case lookupPronoun di name of
            Just prn -> bind prn (Var v) -- todo: aq
            Nothing -> pure ()
        f <- interpretVp vp
        restriction <- f $/ [Var v]
        lift $ Qua det v restriction <$> k (Var v)

interpretNpR :: NpR -> Interpret Tm
interpretNpR (Bound vp) = do
    let name = vpToName vp
    ss <- gets scopes
    case msum $ map (M.lookup name) ss of
        Just tm -> pure tm 
        Nothing -> bindVp Ke (Just vp)
interpretNpR (Ndp (Dp det vp)) = bindVp (unW det) vp
interpretNpR (Ncc (Cc predication cy)) = do
    f <- resetT $ do
        pushScope
        f' <- interpretPredication predication
        popScope
        pure f'
    pure (Evt f)

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
                p1 <- v1 $/ [Evt p2]
                pure $ p1
        "c 1" ->
            TmFun (max 1 l2) h2 frame2 $ \(t:ts) -> do
                p2 <- v2 $/ (t:ts)
                p1 <- v1 $/ [t, Evt p2]
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

lpi :: Dictionary -> Text -> [Formula]
lpi dict text =
    let
        Right tokens = lexToaq text
        Right discourse = parseDiscourse tokens
        cont = interpretDiscourse discourse
    in
        evalState (evalContT cont) (InterpretState [] [M.empty] dict)

lpis :: Text -> IO ()
lpis text = do
    dict <- readDictionary
    mapM_ (T.putStrLn . showFormula) $ lpi dict text
