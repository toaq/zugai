{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Scope where

import Control.Monad (msum)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Lex (Determiner (..))

type VarRef = Text -- like "de poq" or "ta": VP-turned-to-text or pronoun word that refers to a variable

data Scope t = Scope
  { argsSeen :: Int, -- args seen so far in clause: used to make aq decisions
    bindings :: Map VarRef t, -- after "sa dẻ pỏq", generate var "D" and map "de poq" + "ta" to "D" here.
    scopeQuantifiers :: [(Determiner, VarRef, t)],
    scopeFocuses :: [(Text, t)]
  }
  deriving (Eq, Show)

emptyScope :: Scope t
emptyScope = Scope 0 M.empty [] []

class Monad m => HasScopes t m | m -> t where
  getScopes :: m [Scope t]
  setScopes :: [Scope t] -> m ()

pushScope :: HasScopes t m => m ()
pushScope = do
  s <- getScopes
  setScopes (emptyScope : s)

popScope :: HasScopes t m => m (Scope t)
popScope = do
  s <- getScopes
  setScopes (tail s)
  pure (head s)

modifyTop :: HasScopes t m => (Scope t -> Scope t) -> m ()
modifyTop f = do
  s <- getScopes
  setScopes (f (head s) : tail s)

bind :: HasScopes t m => VarRef -> t -> m ()
bind var term = modifyTop (\scope -> scope {bindings = M.insert var term (bindings scope)})

incrementArgsSeen :: HasScopes t m => m ()
incrementArgsSeen = modifyTop (\sc -> sc {argsSeen = argsSeen sc + 1})

resetArgsSeen :: HasScopes t m => m ()
resetArgsSeen = modifyTop (\sc -> sc {argsSeen = 0})

scopeLookup :: HasScopes t m => VarRef -> m (Maybe t)
scopeLookup name =
  msum . map (M.lookup name . bindings) <$> getScopes

quantify :: HasScopes t m => Determiner -> VarRef -> t -> m ()
quantify det var t = do
  modifyTop (\scope -> scope {scopeQuantifiers = ins (scopeQuantifiers scope)})
  where
    isHigh d = d == Ja || d == Hi
    ins qs = case span (\(d, _, _) -> isHigh d >= isHigh det) qs of
      (pre, post) -> pre ++ (det, var, t) : post

focus :: HasScopes t m => Text -> t -> m ()
focus focuser t = do
  modifyTop (\scope -> scope {scopeFocuses = scopeFocuses scope ++ [(focuser, t)]})
