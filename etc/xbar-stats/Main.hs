{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Set (Set)
import Data.Text (Text)
import Xbar (runXbar)
import XbarUtils (Xbar(..), showLabel)
import qualified Data.Set as S
import Dictionary (readDictionary, Dictionary)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Functor.Const (Const(..))
import Data.Functor.Compose (Compose(..))
import Data.Foldable (for_, traverse_)
import Parse (parseDiscourse)
import Data.Either (fromRight)
import Lex (lexToaq)
import Data.Functor ((<&>))
import Data.Function ((&))
import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import Control.Exception (try, SomeException, catch)
import System.IO.Unsafe (unsafePerformIO)
import Control.DeepSeq (force, NFData, ($!!))
import GHC.Generics (Generic)

type LabelName = Text
data Node = Node LabelName [LabelName]
  deriving (Eq, Ord, Generic)

instance NFData Node

main :: IO ()
main = do
  dict <- readDictionary
  sentences <- T.lines <$> T.readFile "test/sentences.txt"
  let nodes = sentences
        & getConst . traverse_ (Const . handleSentence dict)
        & S.toAscList
  for_ nodes $ \(Node lbl xs) -> do
    printf "%s → %s\n" lbl (T.unwords xs)

handleSentence :: Dictionary -> Text -> Set Node
handleSentence dict sent = unsafePerformIO $ do
  let v = sent
        &   eitherToMaybe . lexToaq
        >>= eitherToMaybe . parseDiscourse
        <&> runXbar dict
        <&> xbarStats
  catch (maybe (pure mempty) pure $!! v) $
    \(_ :: SomeException) -> pure mempty

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

xbarStats :: Xbar d -> Set Node
xbarStats = snd . go where
  go :: Xbar d -> (Maybe LabelName, Set Node)
  go = \case
    Tag _i _d lbl x ->
      let ln = showLabel lbl
          (lx, ns) = go x
          this = maybeToSet $ do lx' <- lx; pure $ Node ln [lx']
      in (Just ln, ns <> this)
    Pair _i _d lbl x y ->
      let ln = showLabel lbl
          (lx, ns)  = go x
          (ly, ns') = go y
          this = maybeToSet $ do
            lx' <- lx
            ly' <- ly
            pure $ Node ln [lx', ly']
      in (Just ln, ns <> ns' <> this)
    Leaf {} -> (Nothing, mempty)
    Roof {} -> (Nothing, mempty)

maybeToSet :: Ord a => Maybe a -> Set a
maybeToSet = maybe mempty S.singleton
