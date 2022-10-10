{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Xbar where

-- This module convers zugai parse trees to https://toaq-org.netlify.app/parser/ style *binary* (X-bar) parse trees.

import Control.Monad.State
import Data.Aeson.Micro (object, (.=))
import Data.Aeson.Micro qualified as J
import Data.Char
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace
import Dictionary
import Lex
import Parse
import Scope
import Text.Parsec (SourcePos)
import Text.Parsec.Pos (initialPos)
import TextUtils
import ToSrc
import XbarLabels
import XbarUtils

runXbarWithMovements :: Dictionary -> Discourse -> (Xbar (), Movements)
runXbarWithMovements dict d =
  case runState (unMx (toXbar d)) (XbarState 0 [] emptyMovements dict) of
    (x, s) -> (x, xbarMovements s)

runXbar :: Dictionary -> Discourse -> Xbar ()
runXbar dict d = fst (runXbarWithMovements dict d)

-- Turn n≥1 terms into a parse tree with a "term" or "terms" head.
-- termsToXbar :: Foldable t => t Term -> Mx Xbar
-- termsToXbar = foldl1 (\ma mb -> do a <- ma; b <- mb; mkPair "Terms" a b) . map toXbar . toList

-- When rendering an elided VP after "sa", this "∅" VP is rendered as a fallback.
nullVp :: SourcePos -> Vp
nullVp sourcePos = Single (Nonserial (Vverb (W (Pos sourcePos "" "") [])))

-- Pair a construct with its optional terminator.
terminated :: Category -> Xbar () -> Terminator -> Mx (Xbar ())
terminated _ t Nothing = pure t
terminated cat t (Just word) = do
  xF <- mkTag (Label (X_F cat) []) =<< toXbar word
  mkPair (Label (XP (X_F cat)) []) t xF

covert :: Mx (Xbar ())
covert = mkLeaf (Covert " ") -- I know this is kinda silly... but it's useful that `T.strip` ignores it.

blank :: Mx (Xbar ())
blank = mkLeaf (Covert "")

prenexToXbar :: NonEmpty (Xbar ()) -> W () -> Xbar () -> Mx (Xbar ())
prenexToXbar (x :| []) bi c = do
  xBi <- toXbar bi
  xTopic <- mkTag _Topic xBi
  xTopic' <- mkPair _Topic' xTopic c
  mkPair _TopicP x xTopic'
prenexToXbar (x :| (x' : xs)) bi c = do
  xBi <- mkTag _Topic =<< covert
  xRest <- prenexToXbar (x' :| xs) bi c
  xTopic' <- mkPair _Topic' xBi xRest
  mkPair _TopicP x xTopic'

instance ToXbar Discourse where
  toXbar (Discourse ds) = foldl1 (\ma mb -> do a <- ma; b <- mb; mkPair _SAP a b) (toXbar <$> ds)

instance ToXbar DiscourseItem where
  toXbar (DiSentence x) = toXbar x
  toXbar (DiFragment x) = toXbar x
  toXbar (DiFree x) = toXbar x

instance ToXbar Sentence where
  toXbar (Sentence msc stmt ill) = do
    t <- do
      sa <- maybe covert toXbar ill
      x <- toXbar stmt
      y <- mkTag _SA sa
      mkPair _SAP x y
    case msc of
      Just sc -> do xSConn <- mkTag _SAP =<< toXbar sc; mkPair _SAP xSConn t
      Nothing -> pure t

instance ToXbar Fragment where
  toXbar (FrPrenex (Prenex ts bi)) = do
    xsTopics <- mapM toXbar ts
    prenexToXbar xsTopics bi =<< covert
  toXbar (FrTopic t) = toXbar t

instance ToXbar Statement where
  toXbar (Statement mc mp pred) = do
    pushScope
    xC <- case mc of
      Just c -> toXbar c
      Nothing -> mkTag _C =<< covert
    maybeXsTopicsBi <- case mp of
      Nothing -> pure Nothing
      Just (Prenex ts bi) -> do
        xs <- mapM toXbar ts
        pure (Just (xs, bi))
    xFP <- toXbar pred
    xTopicP <- case maybeXsTopicsBi of
      Just (xs, bi) -> prenexToXbar xs bi xFP
      Nothing -> pure xFP
    s <- popScope
    xWithFoc <- foldrM mkFocAdvP xTopicP (scopeFocuses s)
    xWithQPs <- foldrM mkQP xWithFoc (scopeQuantifiers s)
    if or [d == Ja | (d, _, _) <- scopeQuantifiers s]
      then mkPair _CPλ xC xWithQPs
      else mkPair _CP xC xWithQPs

focGloss :: Text -> Source
focGloss "ku" = Covert "focus, -contrast"
focGloss "bei" = Covert "focus, +contrast"
focGloss "tou" = Covert "only"
focGloss "mao" = Covert "also"
focGloss "juaq" = Covert "even"
focGloss _ = Covert "?"

mkFocAdvP :: (Text, Int) -> Xbar () -> Mx (Xbar ())
mkFocAdvP (focuser, iDP) x = do
  xFocAdv <- mkTag _Foc =<< mkLeaf (focGloss $ bareToaq focuser)
  -- Copy the DP up here... kind of a hack
  xDP <- case subtreeByIndex iDP x of
    Just dp -> mkRoof _DP . Traced =<< aggregateSrc dp
    Nothing -> mkRoof _DP (Traced "???")
  move' iDP (index xDP)
  xFocAdvP <- mkPair _FocP xFocAdv xDP
  mkPair (label x) xFocAdvP x

qgloss :: Determiner -> Text
qgloss Sa = "∃"
qgloss Tu = "∀"
qgloss Tuq = "Λ"
qgloss Ke = "℩"
qgloss Ja = "λ"
qgloss (XShi q) = qgloss q <> "¹"
qgloss _ = "…"

labelIsVP :: Label -> Bool
labelIsVP = (== XP (Head HV)) . labelCategory

mkQP :: (Determiner, VarRef, Int) -> Xbar () -> Mx (Xbar ())
mkQP (det, name, indexOfDPV) x = do
  let isVP = case subtreeByIndex indexOfDPV x of
        Just xDPV -> labelIsVP (label xDPV)
        _ -> False
  xQ <- mkTag _Q =<< mkLeaf (Covert $ qgloss det)
  xV <-
    if isVP
      then mkRoof _VP (Traced name)
      else do vl <- verbLabel name; mkTag vl =<< mkLeaf (Traced name)
  move' indexOfDPV (index xV)
  xQP <- mkPair _QP xQ xV
  mkPair (label x) xQP x

verbClassName :: Maybe VerbClass -> HeadCategory
verbClassName Nothing = HV
verbClassName (Just Tense) = HT
verbClassName (Just Aspect) = HAsp
verbClassName (Just Modality) = HMod
verbClassName (Just Negation) = HNeg

verbLabel :: Text -> Mx Label
verbLabel verbText = do
  d <- gets xbarDictionary
  let mvc = lookupVerbClass d verbText
  pure (Label (Head (verbClassName mvc)) [])

vpcLabel :: VpC -> Mx Label
vpcLabel (Nonserial (Vverb w)) = verbLabel (unW w)
vpcLabel (Nonserial _) = pure $ Label (Head HV) []
vpcLabel (Serial x _) = vpcLabel (Nonserial x)

-- make a V/VP/vP Xbar () out of (verb, NPs) and return xVP.
makeVP :: Xbar () -> [Xbar ()] -> Mx (Xbar ())
makeVP xV xsNp = do
  verb <- aggregateSrc xV
  dict <- gets xbarDictionary
  case xsNp of
    xs
      | Just i <- lookupMaxArity dict verb,
        length xs > i ->
        error $ show verb <> " accepts at most " <> show i <> " argument" <> ['s' | i /= 1]
    [] -> do
      pure xV
    [xDPS] | labelIsVP (label xV) -> do
      -- Our "xV" is actually a VP with an incorporated object.
      xv <- mkTag _v =<< blank
      xv' <- mkPair _v' xv xV
      mkPair _vP xDPS xv'
    [xDPS] -> do
      vname <- verbLabel verb
      let xV' = if labelCategory (label xV) == Head HV then relabel vname xV else xV
      mkPair (Label (XP $ labelCategory vname) []) xV' xDPS
    [xDPS, xDPO] -> do
      xV' <- mkPair _V' xV xDPO
      mkPair _VP xDPS xV'
    [xDPA, xDPS, xDPO] -> do
      xv <- mkTag _v =<< blank
      xV' <- mkPair _V' xV xDPO
      xVP <- mkPair _VP xDPS xV'
      xv' <- mkPair _v' xv xVP
      mkPair _vP xDPA xv'
    _ -> error "verb has too many arguments"

newtype Pro = Pro (Maybe Int) deriving (Eq, Ord, Show)

instance ToXbar Pro where
  toXbar (Pro i) = do
    xDP <- mkTag _DP =<< mkLeaf (Covert "PRO")
    mapM_ (coindex (index xDP)) i
    pure xDP

instance (ToXbar a, ToXbar b) => ToXbar (Either a b) where
  toXbar (Left a) = toXbar a
  toXbar (Right b) = toXbar b

selectCoindex :: Char -> [Xbar ()] -> Maybe Int
selectCoindex 'i' (xi : _) = Just (index xi)
selectCoindex 'i' xs = error $ "couldn't coindex with subject: " <> show (length xs) <> " args"
selectCoindex 'j' (_ : xj : _) = Just (index xj)
selectCoindex 'j' xs = error $ "couldn't coindex with object: " <> show (length xs) <> " args"
selectCoindex 'x' _ = Nothing
selectCoindex c _ = error $ "unrecognized coindex char: " <> show c

-- make a VP for a serial and return (top arity, V to trace into F, VP)
makeVPSerial :: VpC -> [Either Pro Np] -> Mx (Int, Xbar (), Xbar ())
makeVPSerial vp nps = do
  (vNow, xsNpsNow, serialTail) <- case vp of
    Nonserial v -> do
      xsNps <- mapM toXbar nps
      pure (v, xsNps, Nothing)
    Serial v vs -> do
      dict <- gets xbarDictionary
      let frame = fromMaybe "c 0" (lookupFrame dict (toSrc v))
      -- let proCount = frameDigit frame
      let cCount = sum [1 | 'c' <- T.unpack frame]
      xsNpsNow <- (++) <$> mapM toXbar (take cCount nps) <*> replicateM (cCount - length nps) (mkTag _DP =<< covert)
      let ijxs = [c | (d : cs) <- T.unpack <$> T.words frame, isDigit d, c <- cs]
      let pros = [Left (Pro (selectCoindex c xsNpsNow)) | c <- ijxs]
      let npsLater = pros ++ drop cCount nps
      pure (v, xsNpsNow, Just (vs, npsLater))
  xVnow <- mapSrc (Traced . T.toLower . sourceText) <$> toXbar vNow
  xsNpsSerial <- case serialTail of
    Nothing -> pure []
    Just (w, nps) -> do
      (_, xVs, xVPs) <- makeVPSerial w nps
      move xVs xVnow
      pure [xVPs]
  let xsArgs = xsNpsNow ++ xsNpsSerial
  xVPish <- makeVP xVnow xsArgs
  pure (length xsArgs, xVnow, xVPish)

needsVPMovement :: VpC -> Bool
needsVPMovement (Nonserial (Vverb {})) = False
needsVPMovement (Nonserial _) = True
needsVPMovement (Serial n _) = needsVPMovement (Nonserial n)

ensurePhrase :: Category -> Category
ensurePhrase c@(XP _) = c
ensurePhrase c = XP c

instance ToXbar PredicationC where
  toXbar (Predication predicate advsL nps advsR) = do
    xsAdvL <- mapM toXbar advsL
    let v = case predicate of
          Predicate (Single s) -> s
          _ -> error "coordination of verbs not supported in xbar"
    (arity, xVTrace, xVPish) <- makeVPSerial v (map Right nps)
    xFV <-
      if needsVPMovement v
        then do
          mkRoof _Fplus (Overt $ toSrc v)
        else do
          vl <- vpcLabel v
          relabel _Fplus <$> toXbar v
    move xVTrace xFV
    xsAdvR <- mapM toXbar advsR
    let isVPLike label = case labelCategory label of XP _ -> True; _ -> False
    let attachAdverbials (Pair i _ t xL xR) | not (isVPLike t) = do
          xR' <- attachAdverbials xR
          pure (Pair i () t xL xR')
        attachAdverbials x = do
          let lbl = Label (ensurePhrase $ labelCategory $ label x) []
          x' <- foldlM (mkPair lbl) x xsAdvR
          foldrM (mkPair lbl) x' xsAdvL
    xVPa <- attachAdverbials xVPish
    mkPair _FP xFV xVPa

instance ToXbar Predicate where
  toXbar (Predicate vp) = toXbar vp

instance ToXbar Adverbial where
  toXbar (Tadvp t) = toXbar t
  toXbar (Tpp t) = toXbar t

instance ToXbar Topic where
  toXbar (Topicn t) = toXbar t
  toXbar (Topica t) = toXbar t

-- A little helper typeclass to deal with "na" in the Connable' instance below.
-- We just want to handle the types na=() (no "na") and na=(W()) (yes "na") differently in toXbar.
class ToXbarNa na where
  toXbarNa :: Xbar () -> na -> Mx (Xbar ())

instance ToXbarNa () where toXbarNa t () = pure t

instance ToXbarNa (W ()) where
  toXbarNa t na = do
    t1 <- toXbar na
    t2 <- mkTag _CoF t1
    mkPair _CoP t t2

instance (ToXbar t, ToXbarNa na) => ToXbar (Connable' na t) where
  toXbar (Conn x na ru y) = do
    tx <- toXbar x
    t1 <- toXbarNa tx na
    tr <- toXbar ru
    ty <- toXbar y
    t2 <- mkPair _Co' tr ty
    mkPair _CoP t1 t2
  toXbar (ConnTo to ru x to' y) = do
    tt <- mkTag _Co =<< toXbar to
    tr <- toXbar ru
    t1 <- mkPair _Co' tt tr
    tx <- toXbar x
    tt' <- mkTag _Co =<< toXbar to'
    ty <- toXbar y
    t3 <- mkPair _Co' tt' ty
    t2 <- mkPair _CoP tx t3
    mkPair _CoP t1 t2
  toXbar (Single x) = toXbar x

instance ToXbar AdvpC where
  toXbar (Advp t7 verb) = do
    xAdv <- mkTag _Adv =<< mkLeaf (Overt $ posSrc t7)
    xV <- toXbar verb
    mkPair _AdvP xAdv xV

instance ToXbar PpC where
  toXbar (Pp (Single (Prep t6 verb)) np) = do
    xP <- mkTag _P =<< mkLeaf (Overt $ posSrc t6)
    xV <- toXbar verb
    xNP <- toXbar np
    xVP <- mkPair _VP xV xNP
    mkPair _PP xP xVP
  toXbar (Pp _ np) = error "X-bar: coordination of prepositions is unimplemented"

instance ToXbar NpC where
  toXbar (Focused foc np) = do
    xFoc <- mkTag _Foc =<< toXbar foc
    xDP <- toXbar np
    focus (toSrc foc) (index xDP)
    mkPair _FocP xFoc xDP
  toXbar (Unf np) = toXbar np

instance ToXbar NpF where
  toXbar (ArgRel arg rel) = do
    xDP <- toXbar arg
    xCP <- toXbar rel
    mkPair _DP xDP xCP
  toXbar (Unr np) = toXbar np

instance ToXbar NpR where
  toXbar (Npro txt) = do
    xDP <- mkTag _DP =<< mkLeaf (Overt $ inT2 $ unW txt)
    ss <- getScopes
    mi <- scopeLookup (toName txt)
    mapM_ (coindex (index xDP)) mi
    pure xDP
  toXbar (Ndp dp) = toXbar dp
  toXbar (Ncc cc) = toXbar cc

bindVp :: Determiner -> Text -> Int -> Int -> Mx ()
bindVp det name iDP iVP = do
  quantify det name iVP
  bind name iDP
  dictionary <- gets xbarDictionary
  let anaphora = lookupPronoun dictionary name
  case anaphora of
    Just prn -> bind prn iDP
    Nothing -> pure ()

instance ToXbar Dp where
  toXbar (Dp det@(W pos _) maybeVp) = do
    xD <- mkTag _D =<< mkLeaf (Overt $ posSrc pos)
    iDP <- nextNodeNumber
    xVP <- case maybeVp of
      Nothing -> toXbar (nullVp (posPos pos))
      Just vp -> toXbar vp
    let iVP = index xVP
    let name = maybe "" toName maybeVp
    case unW det of
      DT2 -> do
        mi <- scopeLookup name
        case mi of
          Nothing -> pure () -- bindVp Ke name iDP iVP  <-- do this in semantics stage instead.
          Just i -> coindex iDP i
      det -> do
        bindVp det name iDP iVP
    pure $ Pair iDP () _DP xD xVP

instance ToXbar RelC where
  toXbar (Rel pred tmr) = do x <- toXbar pred; terminated (Head HC) x tmr

instance ToXbar Cc where
  toXbar (Cc pred tmr) = do x <- toXbar pred; terminated (Head HC) x tmr

instance ToXbar VpC where
  toXbar (Serial v w) = do
    xV <- toXbar v
    xW <- toXbar w
    serial <- mkPair _V' xV xW
    mkRoof _V . Overt =<< aggregateSrc serial
  toXbar (Nonserial x) = toXbar x

instance ToXbar VpN where
  toXbar (Vname nv name tmr) = do
    xV <- toXbar nv
    xDP <- toXbar name
    xVP <- mkPair _VP xV xDP
    terminated (Head HV) xVP tmr
  toXbar (Vshu shu text) = do
    xV <- mkTag _V =<< toXbar shu
    xDP <- mkTag _DP =<< toXbar text
    mkPair _VP xV xDP
  toXbar (Voiv oiv np tmr) = do
    xCopV <- mkTag _CoP =<< toXbar oiv
    pushScope
    xDP <- toXbar np
    s <- popScope
    let gloss = if unW oiv == "po" then "[hao]" else "[" <> unW oiv <> "ga]"
    xV <- mkTag _V =<< mkLeaf (Covert gloss)
    xV' <- mkPair _V' xV xDP
    xDPpro <- toXbar (Pro Nothing)
    xVP <- mkPair _VP xDPpro xV'
    xWithFoc <- foldrM mkFocAdvP xVP (scopeFocuses s)
    xWithQPs <- foldrM mkQP xWithFoc (scopeQuantifiers s)
    xC <- mkTag _C =<< covert
    xCP <- mkPair _CP xC xWithQPs
    xCopVP <- mkPair _CoP xCopV xCP
    terminated (Head HV) xCopVP tmr
  toXbar (Vmo mo disc teo) = do
    xV <- mkTag _V =<< toXbar mo
    xDiscourse <- toXbar disc
    xVP <- mkPair _VP xV xDiscourse
    terminated (Head HV) xVP teo
  toXbar (Vlu lu stmt ky) = do
    xV <- mkTag _V =<< toXbar lu
    xCP <- toXbar stmt
    case stmt of
      Statement (Just c) _ _ ->
        error $ "lu cannot be followed by an overt complementizer (\"lu " <> T.unpack (toSrc c) <> "\" is invalid)"
      _ -> pure ()
    xVP <- mkPair _VP xV xCP
    terminated (Head HV) xVP ky
  toXbar (Vverb w) = do
    label <- verbLabel (unW w)
    mkTag label =<< toXbar w

instance ToXbar Name where
  toXbar (VerbName x) = relabel _DP <$> toXbar x
  toXbar (TermName x) = relabel _DP <$> toXbar x

instance ToXbar FreeMod where
  toXbar (Fint teto) = mkTag _Free =<< toXbar teto
  toXbar (Fvoc hu np) = do x <- toXbar hu; y <- toXbar np; mkPair _Free x y
  toXbar (Finc ju sentence) = do x <- toXbar ju; y <- toXbar sentence; mkPair _Free x y
  toXbar (Fpar kio disc ki) = do x <- toXbar kio; y <- toXbar disc; z <- toXbar ki; mkPair _Free x =<< mkPair _Free y z

instance ToXbar () where
  toXbar () = mkLeaf (Covert "()")

--instance ToXbar (Pos Text) where
--    toXbar (Pos _ src txt) = Leaf src txt
instance ToXbar t => ToXbar (Pos t) where
  toXbar (Pos _ src t) = do
    inner <- toXbar t
    case inner of
      Leaf _ _ -> mkLeaf (Overt src)
      Tag _ _ t (Leaf _ _) -> mkTag t =<< mkLeaf (Overt src)
      x -> pure x

instance ToXbar t => ToXbar (W t) where
  toXbar (W w fms) = foldl (\ma mb -> do a <- ma; b <- mb; mkPair _FreeP a b) (toXbar w) (toXbar <$> fms)

instance ToXbar Text where toXbar = mkLeaf . Overt

instance ToXbar (Text, Tone) where toXbar (t, _) = mkLeaf (Overt t)

instance ToXbar String where toXbar t = toXbar (T.pack t)

instance ToXbar NameVerb where toXbar nv = mkTag _V =<< toXbar (show nv)

instance ToXbar Determiner where toXbar det = mkTag _D =<< toXbar (show det)

instance ToXbar Connective where toXbar t = mkTag _Co =<< toXbar (show t)

instance ToXbar Complementizer where toXbar t = mkTag _C =<< toXbar (show t)

-- Convert an Xbar () tree to JSON.
xbarToJson :: Maybe (Text -> Text) -> Xbar d -> J.Value
xbarToJson annotate xbar =
  case xbar of
    Leaf {source = src} ->
      object
        [ "type" .= J.String "leaf",
          "src" .= srcToJson src,
          "gloss" .= case annotate of Just f -> srcToJson (mapSource f src); _ -> J.Null
        ]
    Roof {label = t, source = src} ->
      object
        [ "type" .= J.String "roof",
          "tag" .= labelToJson t,
          "src" .= srcToJson src
        ]
    Tag {label = t, child = sub} ->
      object
        [ "type" .= J.String "node",
          "tag" .= labelToJson t,
          "children" .= J.Array [xbarToJson annotate sub]
        ]
    Pair {label = t, leftChild = x, rightChild = y} ->
      object
        [ "type" .= J.String "node",
          "tag" .= labelToJson t,
          "children" .= J.Array (xbarToJson annotate <$> [x, y])
        ]
  where
    srcToJson (Overt t) = J.String t
    srcToJson (Covert t) = J.String t
    srcToJson (Traced t) = J.String t
    labelToJson l = J.String (T.pack $ show l) -- bleh