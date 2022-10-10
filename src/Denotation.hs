{-# LANGUAGE GADTs #-}

module Denotation where

import Data.Text (Text)
import Data.Text qualified as T
import XbarUtils

data SpeechAct
  = Declarative
  | Optative
  deriving (Eq, Ord, Show)

data Quant
  = Exists
  deriving (Eq, Ord, Show)

data Prop
  = PQuant Quant Prop
  deriving (Eq, Ord, Show)

data Thing

-- data Event
data World

data Varname t where
  Vt :: Text -> Varname Thing
  -- Ve :: Text -> Varname Event
  Vw :: Text -> Varname World

data Denotation t where
  DVar :: Varname t -> Denotation t
  DLam :: Varname a -> Denotation t -> Denotation (a -> t)
  DPrp :: Text -> [Denotation t] -> Denotation Bool
  DTru :: Denotation Bool
  DQ :: Denotation ((e -> Bool) -> (e -> Bool) -> Bool)
  DQp :: Quant -> Varname a -> Denotation Bool -> Denotation Bool -> Denotation Bool
  DApp :: Denotation (a -> t) -> Denotation a -> Denotation t
  DSa :: SpeechAct -> Denotation SpeechAct
