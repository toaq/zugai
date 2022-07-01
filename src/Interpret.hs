module Interpret where

import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.Identity
import Control.Monad.State
import Data.Char
import Data.List
import Data.Map qualified as M
import Data.Map (Map)
import Data.Text qualified as T
import Data.Text (Text)

import TextUtils
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
        }
        deriving (Eq, Show)

emptyInterpretState :: InterpretState
emptyInterpretState = InterpretState [] [M.empty]

-- terms
data Tm
    = Var VarName
    | Fus Tm Tm -- X roi Y
    | Quo Text -- quoted text
    | Knd Text -- maybe expand further idk?
    deriving (Eq, Show)

data Formula
    = Prd Text [Tm] -- choq(X,Y,Z)
    | Tru -- tautology
    | Equ Tm Tm
    | Neg Formula
    | Con Connective Formula Formula
    | Qua Determiner VarName Formula Formula -- ∃[x: P(x)] Q(x)
    deriving (Eq, Show)

type Interpret' r = ContT r (State InterpretState)
type Interpret = Interpret' Formula

showCon :: Connective -> Text
showCon Ra = "∨"
showCon Ru = "∧"
showCon Ro = "⊻"
showCon Ri = "?"
showCon Roi = undefined

showQua :: Determiner -> Text
showQua Sa = "∃"
showQua Tuq = "Λ"
showQua Tu = "∀"
showQua Ke = "ι"
showQua q = T.pack $ show q

showTm :: Tm -> Text
showTm (Var v) = v
showTm (Fus x y) = "⌊" <> showTm x <> "," <> showTm y <> "⌉"
showTm (Quo t) = "«" <> t <> "»"
showTm (Knd t) = "baq(" <> t <> ")"

showFormula :: Formula -> Text
showFormula (Prd p ts) = p <> "(" <> T.intercalate "," (showTm <$> ts) <> ")"
showFormula Tru = "⊤"
showFormula (Equ x y) = showTm x <> " = " <> showTm y
showFormula (Neg f) = "¬" <> showFormula f
showFormula (Con c f1 f2) = T.unwords [showFormula f1, showCon c, showFormula f2]
showFormula (Qua det var res f) = "[" <> showQua det <> var <> ": " <> showFormula res <> "] " <> showFormula f

makeFreeVar :: Maybe Text -> Interpret' r Text
makeFreeVar verb = do
    let letters = case verb of
                    Just text | Just (c, _) <- T.uncons text -> [toUpper c]
                    Nothing -> "XYZABCD"
    let candidates = T.pack <$> [h:t | h <- letters, t <- "" : map show [1..]]
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
vpToName (Single (Nonserial (Vverb w))) = bareSrc w
vpToName _ = "xxxtodo"

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
interpretStatement (Statement Nothing rubis) = interpretRubis rubis
interpretStatement (Statement _ rubis) = error "todo prenex"

interpretRubis :: PredicationsRubi -> Interpret Formula
interpretRubis (NonRubi p) = interpretPredication p
interpretRubis _ = error "todo rubi"

interpretPredication :: Predication -> Interpret Formula
interpretPredication (Single p) = interpretPredicationC p
interpretPredication _ = error "todo conn"

interpretPredicationC :: PredicationC -> Interpret Formula
interpretPredicationC (SimplePredication p) = interpretPredicationS p
interpretPredicationC _ = error "todo complementizer"

interpretPredicationS :: PredicationS -> Interpret Formula
interpretPredicationS (Predication (Predicate vp) terms) = do
    f <- interpretVp vp
    ts <- mapM interpretTerm terms
    f (concat ts)

interpretTerm :: Term -> Interpret [Tm]
interpretTerm (Tnp np) = (:[]) <$> interpretNp np
interpretTerm _ = error "term"

interpretNp :: Np -> Interpret Tm
interpretNp (Single npc) = interpretNpC npc
interpretNp _ = error "todo conn"

interpretNpC :: NpC -> Interpret Tm
interpretNpC (Unf npf) = interpretNpF npf
interpretNpC (Focused mao npf) = error "todo focus"

interpretNpF :: NpF -> Interpret Tm
interpretNpF (Unr npr) = interpretNpR npr
interpretNpF (ArgRel arg rel) = error "todo relp"

interpretNpR :: NpR -> Interpret Tm
interpretNpR (Bound vp) = do
    let name = vpToName vp
    ss <- gets scopes
    case msum $ map (M.lookup name) ss of
        Just tm -> pure tm 
        Nothing -> error "todo ke"

interpretNpR (Ndp (Dp (W (Pos _ _ det) _) vp)) = do
    shiftT $ \k -> do
        let name = vpToName <$> vp
        v <- makeFreeVar name
        mapM (\n -> bind n (Var v)) name
        -- todo: bind the anaphora pronoun too
        f <- maybe (\_ -> pure Tru) id <$> mapM interpretVp vp
        restriction <- f [Var v]
        lift $ Qua det v restriction <$> k (Var v)
interpretNpR (Ncc cc) = error "todo cc"

interpretVp :: Vp -> Interpret ([Tm] -> Interpret Formula)
interpretVp (Single vpc) = interpretVpC vpc
interpretVp _ = error "todo conn"

interpretVpC :: VpC -> Interpret ([Tm] -> Interpret Formula)
interpretVpC (Nonserial vpn) = interpretVpN vpn
interpretVpC (Serial vpn vpc) = error "todo serials"

interpretVpN :: VpN -> Interpret ([Tm] -> Interpret Formula)
interpretVpN (Vname nv v ga) = error "todo names"
interpretVpN (Vshu shu (Pos _ src _)) = pure $ \(t:_) -> pure $ Equ (Quo src) t
interpretVpN (Vmo mo txt teo) = pure $ \(t:_) -> pure $ Equ (Quo (T.pack $ show txt)) t
interpretVpN (Voiv oiv np ga) = do
    t <- interpretNp np
    pure $ \ts -> pure $ Prd (bareSrc oiv <> "jeo") (t:ts)
interpretVpN (Vlu lu stmt ky) = do
    pure $ \(t:_) -> do
        pushScope
        bind "hoa" t
        f <- interpretStatement stmt
        popScope
        pure f
interpretVpN (Vverb v) = pure $ \ts -> pure $ Prd (bareSrc v) ts

lpi :: Text -> [Formula]
lpi text =
    let
        Right tokens = lexToaq text
        Right discourse = parseDiscourse tokens
        cont = interpretDiscourse discourse
    in
        evalState (evalContT cont) emptyInterpretState
