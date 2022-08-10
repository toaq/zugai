{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Xbar where

-- This module convers zugai parse trees to https://toaq-org.netlify.app/parser/ style *binary* (X-bar) parse trees.

import Control.Monad.State
import Data.Aeson.Micro ((.=), object)
import Data.Aeson.Micro qualified as J
import Data.Char
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Debug.Trace
import Text.Parsec (SourcePos)
import Text.Parsec.Pos (initialPos)

import Dictionary
import Lex
import Parse
import Scope
import ToSrc
import TextUtils
import XbarUtils

runXbarWithMovements :: Dictionary -> Discourse -> (Xbar, Movements)
runXbarWithMovements dict d =
    case runState (unMx (toXbar d)) (XbarState 0 [] (Movements [] []) dict) of
        (x, s) -> (x, xbarMovements s)

runXbar :: Dictionary -> Discourse -> Xbar
runXbar dict d = fst (runXbarWithMovements dict d)

-- Turn n≥1 terms into a parse tree with a "term" or "terms" head.
-- termsToXbar :: Foldable t => t Term -> Mx Xbar
-- termsToXbar = foldl1 (\ma mb -> do a <- ma; b <- mb; mkPair "Terms" a b) . map toXbar . toList

-- When rendering an elided VP after "sa", this "∅" VP is rendered as a fallback.
nullVp :: SourcePos -> Vp
nullVp sourcePos = Single (Nonserial (Vverb (W (Pos sourcePos "" "") [])))

-- Pair a construct with its optional terminator.
terminated :: Text -> Xbar -> Terminator -> Mx Xbar
terminated _ t Nothing = pure t
terminated tag t (Just word) = mkPair tag t =<< (mkTag "End" =<< toXbar word)

covert :: Mx Xbar
covert = mkLeaf ""

prenexToXbar :: NonEmpty Xbar -> W () -> Xbar -> Mx Xbar
prenexToXbar (x:|[]) bi c = do
    xBi <- toXbar bi
    xTopic <- mkTag "Topic" xBi
    xTopic' <- mkPair "Topic'" xTopic c
    mkPair "TopicP" x xTopic'
prenexToXbar (x:|(x':xs)) bi c = do
    xBi <- mkTag "Topic" =<< covert
    xRest <- prenexToXbar (x':|xs) bi c
    xTopic' <- mkPair "Topic'" xBi xRest
    mkPair "TopicP" x xTopic'

instance ToXbar Discourse where
    toXbar (Discourse ds) = foldl1 (\ma mb -> do a <- ma; b <- mb; mkPair "Discourse" a b) (toXbar <$> ds)
instance ToXbar DiscourseItem where
    toXbar (DiSentence x) = toXbar x
    toXbar (DiFragment x) = toXbar x
    toXbar (DiFree x) = toXbar x
instance ToXbar Sentence where
    toXbar (Sentence msc stmt ill) = do
        t <- do
            sa <- maybe covert toXbar ill
            x <- toXbar stmt
            y <- mkTag "SA" sa
            mkPair "SAP" x y
        case msc of
            Just sc -> do xSConn <- mkTag "SConn" =<< toXbar sc; mkPair "SAP" xSConn t
            Nothing -> pure t
instance ToXbar Fragment where
    toXbar (FrPrenex (Prenex ts bi)) = do
        xsTopics <- mapM toXbar ts
        prenexToXbar xsTopics bi =<< covert
    toXbar (FrTopic t) = toXbar t
instance ToXbar Statement where
    toXbar (Statement mc mp pred) = do
        pushScope
        xC <- case mc of Just c -> toXbar c
                         Nothing -> mkTag "C" =<< covert
        maybeXsTopicsBi <- case mp of
            Nothing -> pure Nothing
            Just (Prenex ts bi) -> do
                xs <- mapM toXbar ts
                pure (Just (xs, bi))
        xFP <- toXbar pred
        xTopicP <- case maybeXsTopicsBi of
            Just (xs,bi) -> prenexToXbar xs bi xFP
            Nothing -> pure xFP
        popScope
        mkPair "CP" xC xTopicP

-- make a V/VP/vP Xbar out of (verb, NPs) and return (xV, xVP).
makeVP :: Maybe VerbClass -> Xbar -> [Xbar] -> Mx (Xbar, Xbar)
makeVP mvc xV xsNp =
    let tamnv = case mvc of Nothing -> "V"
                            Just Tense -> "T"
                            Just Aspect -> "Asp"
                            Just Modality -> "Mod"
                            Just Negation -> "Neg"
    in case xsNp of
        [] -> do
            pure (xV, xV)
        [xDPS] -> do
            xVP <- mkPair (tamnv <> "P") (retag tamnv xV) xDPS
            pure (xV, xVP)
        [xDPS,xDPO] -> do
            xV' <- mkPair "V'" xV xDPO
            xVP <- mkPair "VP" xDPS xV'
            pure (xV, xVP)
        [xDPA,xDPS,xDPO] -> do
            xv <- mkLeaf "𝑣"
            xV' <- mkPair "V'" xV xDPO
            xVP <- mkPair "VP" xDPS xV'
            xv' <- mkPair "𝑣'" xv xVP
            xvP <- mkPair "𝑣P" xDPA xv'
            pure (xV, xvP)
        _ -> error "verb has too many arguments"

data Pro = Pro deriving (Eq, Ord, Show)
instance ToXbar Pro where
    toXbar Pro = mkTag "DP" =<< mkLeaf "PRO"
instance (ToXbar a, ToXbar b) => ToXbar (Either a b) where
    toXbar (Left a) = toXbar a
    toXbar (Right b) = toXbar b

-- make a VP for a serial and return (top arity, V to trace into F, VP)
makeVPSerial :: VpC -> [Either Pro Np] -> Mx (Int, Xbar, Xbar)
makeVPSerial vp nps = do
    (vNow, npsNow, serialTail, serialVerbClass) <- case vp of
        Nonserial v -> pure (v, nps, Nothing, Nothing)
        Serial v vs -> do
            dict <- gets xbarDictionary
            let frame = fromMaybe "c 0" (lookupFrame dict (toSrc v))
            let vc = lookupVerbClass dict (toSrc v)
            let proCount = frameDigit frame
            let cCount = sum [1 | 'c' <- T.unpack frame]
            let npsLater = replicate proCount (Left Pro) ++ drop cCount nps
            pure (v, take cCount nps, Just (vs, npsLater), vc)
    xVnow <- mapSrc T.toLower <$> toXbar vNow
    xsNpsNow <- mapM toXbar npsNow
    xsNpsSerial <- case serialTail of
        Nothing -> pure []
        Just (w, nps) -> do
            (_, xVs, xVPs) <- makeVPSerial w nps
            move xVs xVnow
            pure [xVPs]
    let xsArgs = xsNpsNow ++ xsNpsSerial
    (xVTrace, xVPish) <- makeVP serialVerbClass xVnow xsArgs
    pure (length xsArgs, xVTrace, xVPish)

-- Split a verb complex into tagged TAMs and a non-TAM verb complex.
extractTams :: VpC -> Mx ([(VerbClass, Xbar)], VpC)
extractTams v@(Nonserial _) = pure ([], v)
extractTams v@(Serial vh vt) = do
    d <- gets xbarDictionary
    case lookupVerbClass d (toSrc vh) of
        Nothing -> pure ([], v)
        Just c -> do xV <- toXbar vh; (ts,non) <- extractTams vt; pure ((c,xV):ts,non)

-- Wrap an extracted TAM verb around FP.
wrapTam :: (VerbClass, Xbar) -> Xbar -> Mx Xbar
wrapTam (vc, xV) xFP = do
    (_, xTamP) <- makeVP (Just vc) xV [xFP]
    pure xTamP

instance ToXbar PredicationC where
    toXbar (Predication predicate advsL nps advsR) = do
        xsAdvL <- mapM toXbar advsL
        xsAdvR <- mapM toXbar advsR
        let attachAdverbials nodeName x = do
                x' <- foldlM (mkPair nodeName) x xsAdvR
                foldrM (mkPair nodeName) x' xsAdvL

        let Predicate (Single v) = predicate
        -- Extract TAMs so we can wrap them around FP later.
        (xsTam, nonTam) <- extractTams v
        (arity, xVTrace, xVPish) <- makeVPSerial nonTam (map Right nps)
        let fvtag = if arity >= 3 then "F+𝑣+V" else "F+V"
        let attachP = if arity >= 3 then "𝑣P" else "VP"
        xFV <- retag fvtag <$> toXbar nonTam
        move xVTrace xFV
        xVPa <- attachAdverbials attachP xVPish
        xFP <- mkPair "FP" xFV xVPa
        foldrM wrapTam xFP xsTam

instance ToXbar Predicate where
    toXbar (Predicate vp) = toXbar vp
instance ToXbar Adverbial where
    toXbar (Tadvp t) = toXbar t
    toXbar (Tpp t) = toXbar t
instance ToXbar Topic where
    toXbar (Topicn t) = toXbar t
    toXbar (Topica t) = toXbar t

-- Typeclass for associating a "connectand name" with a type
-- so that we can generate strings like Co(NP), Co(VP), etc. in the generic Connable' instance.
class ConnName t where connName :: Text
instance ConnName NpC where connName = "NP"
instance ConnName VpC where connName = "V"
instance ConnName PredicationC where connName = "Pred"
instance ConnName PrepC where connName = "P"
instance ConnName RelC where connName = "Rel"
instance ConnName AdvpC where connName = "AdvP"
instance ConnName PpC where connName = "PP"

-- A little helper typeclass to deal with "na" in the Connable' instance below.
-- We just want to handle the types na=() (no "na") and na=(W()) (yes "na") differently in toXbar.
class ToXbarNa na where
    toXbarNa :: Xbar -> na -> Mx Xbar

instance ToXbarNa () where toXbarNa t () = pure t
instance ToXbarNa (W ()) where
    toXbarNa t na = do
        t1 <- toXbar na
        t2 <- mkTag "End" t1
        mkPair "CoP" t t2

instance (ToXbar t, ToXbarNa na, ConnName t) => ToXbar (Connable' na t) where
    toXbar (Conn x na ru y) = do
        tx <- toXbar x
        t1 <- toXbarNa tx na
        tr <- toXbar ru
        ty <- toXbar y
        t2 <- mkPair "Co'" tr ty
        mkPair ("CoP(" <> connName @t <> ")") t1 t2
    toXbar (ConnTo to ru x to' y) = do
        tt <- mkTag "Co" =<< toXbar to
        tr <- toXbar ru
        t1 <- mkPair "Co'" tt tr
        tx <- toXbar x
        tt' <- mkTag "Co" =<< toXbar to'
        ty <- toXbar y
        t3 <- mkPair "Co'" tt' ty
        t2 <- mkPair ("CoP(" <> connName @t <> ")") tx t3
        mkPair ("CoP(" <> connName @t <> ")") t1 t2
    toXbar (Single x) = toXbar x

instance ToXbar AdvpC where
    toXbar (Advp t7 verb) = do
        xAdv <- mkTag "Adv" =<< mkLeaf (posSrc t7)
        xV <- toXbar verb
        mkPair "AdvP" xAdv xV

instance ToXbar PpC where
    toXbar (Pp (Single (Prep t6 verb)) np) = do
        xP <- mkTag "P" =<< mkLeaf (posSrc t6)
        xV <- toXbar verb
        xNP <- toXbar np
        xVP <- mkPair "VP" xV xNP
        mkPair "PP" xP xVP
    toXbar (Pp _ np) = error "X-bar: coordination of prepositions is unimplemented"

instance ToXbar NpC where
    toXbar (Focused foc np) = do x<-toXbar foc; y<-toXbar np; mkPair "Foc" x y
    toXbar (Unf np) = toXbar np
instance ToXbar NpF where
    toXbar (ArgRel arg rel) = do x<-toXbar arg; y<-toXbar rel; mkPair "DP" x y
    toXbar (Unr np) = toXbar np
instance ToXbar NpR where
    toXbar (Npro txt) = do
        xDP <- mkTag "DP" =<< mkLeaf (inT2 $ unW txt)
        ss <- getScopes
        mi <- scopeLookup (toName txt)
        mapM_ (coindex (index xDP)) mi
        pure xDP
    toXbar (Ndp dp) = toXbar dp
    toXbar (Ncc cc) = toXbar cc

bindVp :: Text -> Int -> Mx ()
bindVp name idx = do
    bind name idx
    dictionary <- gets xbarDictionary
    let anaphora = lookupPronoun dictionary name
    case anaphora of
        Just prn -> bind prn idx
        Nothing -> pure ()

instance ToXbar Dp where
    toXbar (Dp det@(W pos _) maybeVp) = do
        xD <- mkTag "D" =<< mkLeaf (posSrc pos)
        iDP <- nextNodeNumber
        xVP <- case maybeVp of
            Nothing -> toXbar (nullVp (posPos pos))
            Just vp -> do
                case unW det of
                    DT2 -> do
                        mi <- scopeLookup (toName vp)
                        case mi of
                            Nothing -> bindVp (toName vp) iDP
                            Just i -> coindex iDP i
                    _ -> do
                        bindVp (toName vp) iDP
                ss <- getScopes
                toXbar vp
        pure $ Pair iDP "DP" xD xVP
instance ToXbar RelC where
    toXbar (Rel pred tmr) = do x <- toXbar pred; terminated "CP" x tmr
instance ToXbar Cc where
    toXbar (Cc pred tmr) = do x <- toXbar pred; terminated "CP" x tmr
instance ToXbar VpC where
    toXbar (Serial v w) = do
        xV <- toXbar v
        xW <- toXbar w
        serial <- mkPair "Serial" xV xW
        mkRoof "V" (aggregateSrc serial)
    toXbar (Nonserial x) = toXbar x
instance ToXbar VpN where
    toXbar (Vname nv name tmr) = do
        t1 <- toXbar nv
        t2 <- toXbar name
        t3 <- mkPair "Vname" t1 t2
        terminated "Vname" t3 tmr
    toXbar (Vshu shu text) = do
        t1 <- mkTag "Quoter" =<< toXbar shu
        t2 <- mkTag "Quoted" =<< toXbar text
        mkPair "Vquote" t1 t2
    toXbar (Voiv oiv np tmr) = do
        t1 <- mkTag "OIV" =<< toXbar oiv
        t2 <- toXbar np
        t3 <- mkPair "Vinc" t1 t2
        terminated "Vinc" t3 tmr
    toXbar (Vmo mo disc teo) = do
        t1 <- mkTag "Quoter" =<< toXbar mo
        t2 <- toXbar disc
        t3 <- mkPair "Vquote" t1 t2
        terminated "Vquote" t3 teo
    toXbar (Vlu lu stmt ky) = do
        t1 <- mkTag "Free" =<< toXbar lu
        t2 <- toXbar stmt
        t3 <- mkPair "Vfree" t1 t2
        terminated "Vfree" t3 ky
    toXbar (Vverb w) = mkTag "V" =<< toXbar w
instance ToXbar Name where
    toXbar (VerbName x) = toXbar x
    toXbar (TermName x) = toXbar x
instance ToXbar FreeMod where
    toXbar (Fint teto) = mkTag "Interj" =<< toXbar teto
    toXbar (Fvoc hu np) = do x<-toXbar hu; y<-toXbar np; mkPair "Voc" x y
    toXbar (Finc ju sentence) = do x<-toXbar ju; y<-toXbar sentence; mkPair "Inc" x y
    toXbar (Fpar kio disc ki) = do x<-toXbar kio; y<-toXbar disc; z<-toXbar ki; mkPair "Par" x =<< mkPair "Par" y z
instance ToXbar () where
    toXbar () = mkLeaf "()"
--instance ToXbar (Pos Text) where
--    toXbar (Pos _ src txt) = Leaf src txt
instance ToXbar t => ToXbar (Pos t) where
    toXbar (Pos _ src t) = do
        inner <- toXbar t
        case inner of
            Leaf _ _ -> mkLeaf src
            Tag _ t (Leaf _ _) -> mkTag t =<< mkLeaf src
            x -> pure x
instance ToXbar t => ToXbar (W t) where
    toXbar (W w fms) = foldl (\ma mb -> do a<-ma; b<-mb; mkPair "Free" a b) (toXbar w) (toXbar <$> fms)
instance ToXbar Text where toXbar = mkLeaf
instance ToXbar (Text, Tone) where toXbar (t, _) = mkLeaf t
instance ToXbar String where toXbar t = toXbar (T.pack t)

instance ToXbar NameVerb where toXbar nv = mkTag "NameVerb" =<< toXbar (show nv)
instance ToXbar Determiner where toXbar det = mkTag "D" =<< toXbar (show det)
instance ToXbar Connective where toXbar t = mkTag "Co" =<< toXbar (show t)
instance ToXbar Complementizer where toXbar t = mkTag "C" =<< toXbar (show t)

-- Show an Xbar tree using indentation and ANSI colors.
showXbarAnsi :: Xbar -> [Text]
showXbarAnsi (Leaf _ src) = ["\x1b[94m" <> src <> "\x1b[0m"]
showXbarAnsi (Roof _ t src) = ["\x1b[95m" <> t <> ": " <> src <> "\x1b[0m"]
showXbarAnsi (Tag _ t sub) =
    case showXbarAnsi sub of
        [one] -> [t <> ": " <> one]
        many -> (t<>":") : map ("  "<>) many
showXbarAnsi (Pair _ t x y) = (t<>":") : map ("  "<>) (showXbarAnsi x) ++ map ("  "<>) (showXbarAnsi y)

-- Convert an Xbar tree to JSON.
xbarToJson :: Maybe (Text -> Text) -> Xbar -> J.Value
xbarToJson annotate xbar =
    case xbar of
        Leaf _ src -> object ["type" .= J.String "leaf", "src" .= J.String src, "gloss" .= case annotate of Just f -> J.String (f src); _ -> J.Null]
        Roof _ t src -> object ["type" .= J.String "roof", "tag" .= J.String t, "src" .= J.String src]
        Tag _ t sub -> object ["type" .= J.String "node", "tag" .= J.String t, "children" .= J.Array [xbarToJson annotate sub]]
        Pair _ t x y -> object ["type" .= J.String "node", "tag" .= J.String t, "children" .= J.Array (xbarToJson annotate <$> [x,y])]
