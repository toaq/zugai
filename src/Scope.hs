{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Scope where

import Data.Text (Text)
import Data.Map (Map)
import Data.Map qualified as M

type VarRef = Text -- like "de poq" or "ta": VP-turned-to-text or pronoun word that refers to a variable
data Scope t =
    Scope
        { argsSeen :: Int  -- args seen so far in clause: used to make aq decisions
        , bindings :: Map VarRef t  -- after "sa dẻ pỏq", generate var "D" and map "de poq" + "ta" to "D" here.
        } deriving (Eq, Show)

emptyScope :: Scope t
emptyScope = Scope 0 M.empty

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
bind var term = modifyTop (\scope -> scope { bindings = M.insert var term (bindings scope) })

incrementArgsSeen :: HasScopes t m => m ()
incrementArgsSeen = modifyTop (\(Scope i s) -> Scope (i+1) s)
