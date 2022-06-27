{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

module Tree where

-- This module convers zugai parse trees to https://toaq-org.netlify.app/parser/ style *binary* parse trees.

import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Lex
import Parse
import Text.Parsec (SourcePos)

data Tree
    = Tag Text Tree
    | Pair Text Tree Tree
    | Leaf Text -- Source word and gloss
    deriving (Eq, Show)

class ToTree a where
    toTree :: a -> Tree

-- Turn n≥1 terms into a parse tree with a "term" or "terms" head.
termsToTree :: Foldable t => t Term -> Tree
termsToTree = foldr1 (Pair "terms") . map toTree . toList

-- When rendering an elided VP after "sa", this "∅" VP is rendered as a fallback.
nullVp :: SourcePos -> Vp
nullVp sourcePos = Single (Nonserial (Vverb (W (Pos sourcePos "∅" "∅") [])))

-- Pair a construct with its optional terminator.
toTreeTerminator :: Text -> Tree -> Terminator -> Tree
toTreeTerminator _ t Nothing = t
toTreeTerminator tag t (Just word) = Pair tag t (Tag "terminator" $ toTree word)

instance ToTree Discourse where
    toTree (Discourse ds) = foldr1 (Pair "discourse") (toTree <$> ds)
instance ToTree DiscourseItem where
    toTree (DiSentence x) = toTree x
    toTree (DiFragment x) = toTree x
    toTree (DiFree x) = toTree x
instance ToTree Sentence where
    toTree (Sentence sc stmt ill) =
        let t = case ill of Just i -> Pair "S" (toTree stmt) (Tag "illocution" $ toTree i)
                            Nothing -> toTree stmt
        in case sc of Just sc -> Pair "S" (Tag "connector" $ toTree sc) t
                      Nothing -> t
instance ToTree Fragment where
    toTree (FrPrenex x) = toTree x
    toTree (FrTerms ts) = termsToTree ts
instance ToTree Prenex where
    toTree (Prenex ts bi) = Pair "prenex" (termsToTree ts) (Tag "end" $ toTree bi)
instance ToTree Statement where
    toTree (Statement (Just prenex) preds) = Pair "statement" (toTree prenex) (toTree preds)
    toTree (Statement Nothing preds) = toTree preds
instance ToTree PredicationsRubi where
    toTree (Rubi p1 ru bi ps) = Pair "rubis" (Pair "rubi" (toTree p1) (Pair "Co" (toTree ru) (toTree bi))) (toTree ps)
    toTree (NonRubi p) = toTree p
instance ToTree PredicationC where
    toTree (CompPredication comp stmt) = Pair "comp-pred" (toTree comp) (toTree stmt)
    toTree (SimplePredication pred) = toTree pred
instance ToTree PredicationS where
    toTree (Predication predicate []) = Tag "predication" (toTree predicate)
    toTree (Predication predicate terms) = Pair "predication" (toTree predicate) (termsToTree terms)
instance ToTree Predicate where
    toTree (Predicate vp) = Tag "predicate" (toTree vp)
instance ToTree Term where
    toTree (Tnp t) = toTree t
    toTree (Tadvp t) = toTree t
    toTree (Tpp t) = toTree t
    toTree (Termset to ru t1 to' t2) =
        Pair "termset"
            (Pair "forethought" (toTree to) (toTree ru))
            (Pair "CoP(termset)" (termsToTree t1)
                (Pair "Co'" (toTree to') (termsToTree t2)))

-- Typeclass for associating a "connectand name" with a type
-- so that we can generate strings like Co(NP), Co(VP), etc. in the generic Connable' instance.
class ConnName t where connName :: Text
instance ConnName NpC where connName = "NP"
instance ConnName VpC where connName = "VP"
instance ConnName PredicationC where connName = "Pred"
instance ConnName PrepC where connName = "Prep"
instance ConnName RelC where connName = "Rel"
instance ConnName AdvpC where connName = "AdvP"
instance ConnName PpC where connName = "PrepP"

-- A little helper typeclass to deal with "na" in the Connable' instance below.
-- We just want to handle the types na=() (no "na") and na=(W()) (yes "na") differently in toTree.
class ToTreeNa na where
    toTreeNa :: Tree -> na -> Tree

instance ToTreeNa () where toTreeNa t () = t
instance ToTreeNa (W ()) where toTreeNa t na = Pair "CoP" t (Tag "terminator" $ toTree na)

instance (ToTree t, ToTreeNa na, ConnName t) => ToTree (Connable' na t) where
    toTree (Conn x na ru y) =
        Pair ("CoP(" <> connName @t <> ")")
            (toTreeNa (toTree x) na)
            (Pair "Co'" (toTree ru) (toTree y))
    toTree (ConnTo to ru x to' y) =
        Pair ("forethoughtCoP(" <> connName @t <> ")")
            (Pair "forethought" (Tag "marker" $ toTree to) (toTree ru))
            (Pair ("CoP(" <> connName @t <> ")") (toTree x)
                (Pair "Co'" (Tag "marker" $ toTree to') (toTree y)))
    toTree (Single x) = toTree x

instance ToTree AdvpC where
    toTree (Advp vp) = Tag "AdvP" (toTree vp)
instance ToTree PpC where
    toTree (Pp prep np) = Pair "PrepP" (toTree prep) (toTree np)
instance ToTree PrepC where
    toTree (Prep vp) = Tag "preposition" (toTree vp)
instance ToTree NpC where
    toTree (Focused foc np) = Pair "focus" (toTree foc) (toTree np)
    toTree (Unf np) = toTree np
instance ToTree NpF where
    toTree (ArgRel arg rel) = Pair "argrel" (toTree arg) (toTree rel)
    toTree (Unr np) = toTree np
instance ToTree NpR where
    toTree (Bound vp) = Tag "bound" $ toTree vp
    toTree (Ndp dp) = toTree dp
    toTree (Ncc cc) = toTree cc
instance ToTree Dp where
    toTree (Dp det@(W pos _) vp) = Pair "DP" (Tag "determiner" $ Leaf (posSrc pos)) (toTree $ maybe (nullVp $ posPos pos) id vp)
instance ToTree RelC where
    toTree (Rel pred tmr) = toTreeTerminator "relative" (toTree pred) tmr
instance ToTree Cc where
    toTree (Cc pred tmr) = toTreeTerminator "content" (toTree pred) tmr
instance ToTree VpC where
    toTree (Serial x y) = Pair "serial" (toTree x) (toTree y)
    toTree (Nonserial x) = toTree x
instance ToTree VpN where
    toTree (Vname nv name tmr) = toTreeTerminator "name" (Pair "name" (toTree $ show nv) (toTree name)) tmr
    toTree (Vshu shu text) = Pair "quote" (Tag "quoter" $ toTree shu) (Tag "text" $ toTree text)
    toTree (Voiv oiv np tmr) = toTreeTerminator "OIV" (Pair "OIV" (toTree oiv) (toTree np)) tmr
    toTree (Vmo mo disc teo) = toTreeTerminator "quote" (Pair "quote" (Tag "quoter" $ toTree mo) (toTree disc)) teo
    toTree (Vlu lu stmt ky) = toTreeTerminator "freerel" (Pair "freerel" (Tag "predicatizer" $ toTree lu) (toTree stmt)) ky
    toTree (Vverb w) = Tag "verb" (toTree w)
instance ToTree Name where
    toTree (VerbName x) = toTree x
    toTree (TermName x) = toTree x
instance ToTree FreeMod where
    toTree (Fint teto) = Tag "interjection" (toTree teto)
    toTree (Fvoc hu np) = Pair "vocative" (toTree hu) (toTree np)
    toTree (Finc ju sentence) = Pair "incidental" (toTree ju) (toTree sentence)
    toTree (Fpar kio disc ki) = Pair "parenthetical" (toTree kio) (Pair "parenthetical" (toTree disc) (toTree ki))
instance ToTree () where
    toTree () = Leaf "()"
--instance ToTree (Pos Text) where
--    toTree (Pos _ src txt) = Leaf src txt
instance ToTree t => ToTree (Pos t) where
    toTree (Pos _ src t) =
        case toTree t of
            Leaf _ -> Leaf src
            Tag t (Leaf _) -> Tag t (Leaf src)
            x -> x
instance ToTree t => ToTree (W t) where
    toTree (W w fms) = foldl (Pair "freemod") (toTree w) (toTree <$> fms)
instance ToTree Text where toTree t = Leaf t
instance ToTree (Text, Tone) where toTree (t, _) = Leaf t
instance ToTree String where toTree t = toTree (T.pack t)

instance ToTree Determiner where toTree det = Tag "determiner" $ toTree $ show det
instance ToTree Connective where toTree t = Tag "connective" $ toTree $ show t
instance ToTree Complementizer where toTree t = Tag "complementizer" $ toTree $ show t

-- Cut redundant one-child nodes out of a tree.
-- predication(predicate(verb(de))) becomes just verb(de).
simplifyTree :: Tree -> Tree
simplifyTree t@(Leaf _) = t
simplifyTree t@(Tag _ (Leaf _)) = t
simplifyTree (Tag _ sub) = simplifyTree sub
simplifyTree (Pair t x y) = Pair t (simplifyTree x) (simplifyTree y)

-- Show a tree using indentation and ANSI colors.
showTree :: Tree -> [Text]
showTree (Leaf src) = ["\x1b[94m" <> src <> "\x1b[0m"]
showTree (Tag t sub) =
    case showTree sub of
        [one] -> [t <> ": " <> one]
        many -> (t<>":") : map ("  "<>) many
showTree (Pair t x y) = (t<>":") : map ("  "<>) (showTree x) ++ map ("  "<>) (showTree y)
