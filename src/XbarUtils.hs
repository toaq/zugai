{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module XbarUtils where

import Control.Monad.State
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Dictionary
import Lex
import Parse
import Scope
import TextUtils
import ToSrc

data Feature = PlusLambda deriving (Eq, Ord, Show)

data HeadCategory
  = HAdv
  | HAsp
  | HC
  | HCo
  | HD
  | HF
  | HFoc
  | HFree
  | HMod
  | HNeg
  | HP
  | HQ
  | HSA
  | HT
  | HTopic
  | Hv
  | HV
  deriving (Eq, Ord, Show)

data Category
  = Head HeadCategory
  | X_F Category -- terminator
  | X' Category
  | XP Category
  | Xplus Category
  deriving (Eq, Ord, Show)

data Label = Label
  { labelCategory :: Category,
    labelFeatures :: [Feature]
  }
  deriving (Eq, Ord, Show)

showLabel :: Label -> Text
showLabel (Label cat fs) = showCat cat <> T.concat (showFeature <$> fs)
  where
    showCat (Head h) = showHead h
    showCat (X_F c) = showCat c <> "_F"
    showCat (X' c) = showCat c <> "'"
    showCat (XP c) = showCat c <> "P"
    showCat (Xplus c) = showCat c <> "+"
    showHead = T.tail . T.pack . show
    showFeature PlusLambda = "[+λ]"

data Source
  = Overt Text
  | Covert Text
  | Traced Text
  deriving (Eq, Show)

sourceText :: Source -> Text
sourceText (Overt t) = t
sourceText (Covert t) = t
sourceText (Traced t) = t

mapSource :: (Text -> Text) -> Source -> Source
mapSource f (Overt t) = Overt (f t)
mapSource f (Covert t) = Covert (f t)
mapSource f (Traced t) = Covert (f t)

data Xbar d
  = Tag {index :: Int, denotation :: d, label :: Label, child :: Xbar d}
  | Pair {index :: Int, denotation :: d, label :: Label, leftChild :: Xbar d, rightChild :: Xbar d}
  | Leaf {index :: Int, source :: Source} -- Source word
  | Roof {index :: Int, denotation :: d, label :: Label, source :: Source} -- Tag and source text
  deriving (Eq, Show)

indices :: Xbar d -> [Int]
indices Tag {index = i, child = x} = i : indices x
indices Pair {index = i, leftChild = x, rightChild = y} = i : indices x ++ indices y
indices Leaf {index = i} = [i]
indices Roof {index = i} = [i]

indicesBelow :: [Int] -> Xbar d -> [Int]
indicesBelow is Tag {index = i, child = x} = if i `elem` is then indices x else indicesBelow is x
indicesBelow is Pair {index = i, leftChild = x, rightChild = y} = if i `elem` is then indices x ++ indices y else indicesBelow is x ++ indicesBelow is y
indicesBelow is Leaf {index = i} = [i | i `elem` is] -- not really "below" but... it's useful for little v movement
indicesBelow is Roof {index = i} = [i | i `elem` is]

subtrees :: Xbar d -> [Xbar d]
subtrees t@Tag {child = x} = t : subtrees x
subtrees t@Pair {leftChild = x, rightChild = y} = t : subtrees x ++ subtrees y
subtrees t = [t]

subtreeByIndex :: Int -> Xbar d -> Maybe (Xbar d)
subtreeByIndex i x = listToMaybe [t | t <- subtrees x, index t == i]

relabel :: Label -> Xbar d -> Xbar d
relabel t x = x {label = t}

overtText :: Source -> Text
overtText (Overt t) = t
overtText _ = ""

onOvert :: (Text -> Text) -> Source -> Source
onOvert f (Overt t) = Overt (f t)
onOvert f s = s

mapSrc :: (Source -> Source) -> Xbar d -> Xbar d
mapSrc f (Tag i d l x) = Tag i d l (mapSrc f x)
mapSrc f (Pair i d l x y) = Pair i d l (mapSrc f x) (mapSrc f y)
mapSrc f (Leaf i s) = Leaf i (f s)
mapSrc f (Roof i d l s) = Roof i d l (f s)

aggregateSrc :: Xbar d -> Mx Text
aggregateSrc x =
  do
    let go :: Xbar d -> Text
        go (Tag _ _ _ x) = go x
        go (Pair _ _ _ x y) = combineWords (go x) (go y)
        go (Leaf i s) = overtText s
        go (Roof i _ _ s) = overtText s
    pure $ go x

data Movement = Movement
  { movementSource :: Int,
    movementTarget :: Int
  }
  deriving (Eq, Ord, Show)

data Movements = Movements
  { movements :: [Movement],
    coindexations :: [(Int, Int)]
  }
  deriving (Eq, Show)

emptyMovements :: Movements
emptyMovements = Movements [] []

data XbarState = XbarState
  { xbarNodeCounter :: Int,
    xbarScopes :: [Scope Int],
    xbarMovements :: Movements,
    xbarDictionary :: Dictionary
  }
  deriving (Eq, Show)

newtype Mx a = Mx
  { unMx :: State XbarState a
  }
  deriving (Functor, Applicative, Monad, MonadState XbarState)

instance HasScopes Int Mx where
  getScopes = gets xbarScopes
  setScopes ss = modify (\xs -> xs {xbarScopes = ss})

nextNodeNumber :: Mx Int
nextNodeNumber = do
  i <- gets xbarNodeCounter
  modify (\s -> s {xbarNodeCounter = i + 1})
  pure i

class ToXbar a where
  toXbar :: a -> Mx (Xbar ())

mkTag :: Label -> Xbar () -> Mx (Xbar ())
mkTag l x = do i <- nextNodeNumber; pure $ Tag i () l x

mkPair :: Label -> Xbar () -> Xbar () -> Mx (Xbar ())
mkPair l x y = do i <- nextNodeNumber; pure $ Pair i () l x y

mkLeaf :: Source -> Mx (Xbar ())
mkLeaf s = do i <- nextNodeNumber; pure $ Leaf i s

mkRoof :: Label -> Source -> Mx (Xbar ())
mkRoof l s = do i <- nextNodeNumber; pure $ Roof i () l s

mkCopy :: Xbar () -> Mx (Xbar ())
mkCopy (Tag _ () l x) = do i <- nextNodeNumber; Tag i () l <$> mkCopy x
mkCopy (Pair _ () l x y) = do i <- nextNodeNumber; Pair i () l <$> mkCopy x <*> mkCopy y
mkCopy (Leaf _ s) = do i <- nextNodeNumber; pure $ Leaf i s
mkCopy (Roof _ () l s) = do i <- nextNodeNumber; pure $ Roof i () l s

move' :: Int -> Int -> Mx ()
move' i j =
  modify
    ( \s ->
        let ms = xbarMovements s
         in s {xbarMovements = ms {movements = Movement i j : movements ms}}
    )

move :: Xbar d -> Xbar d -> Mx ()
move src tgt = move' (index src) (index tgt)

coindex :: Int -> Int -> Mx ()
coindex i j =
  modify
    ( \s ->
        let ms = xbarMovements s
         in s {xbarMovements = ms {coindexations = (i, j) : coindexations ms}}
    )

coindexationNames :: [(Int, Int)] -> Map Int Char
coindexationNames coixs = go 'i' M.empty $ sortOn (uncurry min) coixs
  where
    go c m [] = m
    go c m ((i, j) : xs) = case (m M.!? i, m M.!? j) of
      (Nothing, Nothing) -> go (succ c) (M.insert i c (M.insert j c m)) xs
      (Just z, _) -> go c (M.insert j z m) xs
      (_, Just z) -> go c (M.insert i z m) xs

getCoindexationName :: [(Int, Int)] -> Int -> Maybe Text
getCoindexationName coixs =
  let cn = coindexationNames coixs in fmap T.singleton . (cn M.!?)
