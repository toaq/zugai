{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module XbarUtils where

import Control.Monad.State
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Dictionary
import Lex
import Parse
import Scope
import TextUtils
import ToSrc

data Xbar
  = Tag Int Text Xbar
  | Pair Int Text Xbar Xbar
  | Leaf Int Text -- Source word
  | Roof Int Text Text -- Tag and source text
  deriving (Eq, Show)

index :: Xbar -> Int
index (Tag i _ _) = i
index (Pair i _ _ _) = i
index (Leaf i _) = i
index (Roof i _ _) = i

indices :: Xbar -> [Int]
indices (Tag i _ x) = i : indices x
indices (Pair i _ x y) = i : indices x ++ indices y
indices (Leaf i _) = [i]
indices (Roof i _ _) = [i]

indicesBelow :: [Int] -> Xbar -> [Int]
indicesBelow is (Tag i _ x) = if i `elem` is then indices x else indicesBelow is x
indicesBelow is (Pair i _ x y) = if i `elem` is then indices x ++ indices y else indicesBelow is x ++ indicesBelow is y
indicesBelow is (Leaf i _) = [i | i `elem` is] -- not really "below" but... it's useful for little v movement
indicesBelow is (Roof i _ _) = [i | i `elem` is]

retag :: Text -> Xbar -> Xbar
retag t (Tag i _ x) = Tag i t x
retag t (Pair i _ x y) = Pair i t x y
retag _ x@(Leaf _ _) = x
retag t (Roof i _ s) = Roof i t s

mapSrc :: (Text -> Text) -> Xbar -> Xbar
mapSrc f (Tag i t x) = Tag i t (mapSrc f x)
mapSrc f (Pair i t x y) = Pair i t (mapSrc f x) (mapSrc f y)
mapSrc f (Leaf i s) = Leaf i (f s)
mapSrc f (Roof i t s) = Roof i t (f s)

aggregateSrc :: Xbar -> Text
aggregateSrc (Tag _ _ x) = aggregateSrc x
aggregateSrc (Pair _ _ x y) = combineWords (aggregateSrc x) (aggregateSrc y)
aggregateSrc (Leaf _ s) = s
aggregateSrc (Roof _ _ s) = s

data Movement = Movement
  { movementSource :: Int,
    movementTarget :: Int
  }
  deriving (Eq, Ord, Show)

data Movements = Movements
  { movements :: [Movement],
    coindexations :: [(Int, Int)],
    traces :: [Int]
  }
  deriving (Eq, Show)

emptyMovements :: Movements
emptyMovements = Movements [] [] []

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
  toXbar :: a -> Mx Xbar

mkTag :: Text -> Xbar -> Mx Xbar
mkTag t x = do i <- nextNodeNumber; pure $ Tag i t x

mkPair :: Text -> Xbar -> Xbar -> Mx Xbar
mkPair t x y = do i <- nextNodeNumber; pure $ Pair i t x y

mkLeaf :: Text -> Mx Xbar
mkLeaf t = do i <- nextNodeNumber; pure $ Leaf i t

mkRoof :: Text -> Text -> Mx Xbar
mkRoof t s = do i <- nextNodeNumber; pure $ Roof i t s

move :: Xbar -> Xbar -> Mx ()
move src tgt =
  let m = Movement (index src) (index tgt)
   in modify
        ( \s ->
            let ms = xbarMovements s
             in s {xbarMovements = ms {movements = m : movements ms}}
        )

coindex :: Int -> Int -> Mx ()
coindex i j =
  modify
    ( \s ->
        let ms = xbarMovements s
         in s {xbarMovements = ms {coindexations = (i, j) : coindexations ms}}
    )

traceAt :: Xbar -> Mx ()
traceAt x =
  modify
    ( \s ->
        let ms = xbarMovements s
         in s {xbarMovements = ms {traces = index x : traces ms}}
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
