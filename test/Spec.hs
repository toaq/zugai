import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Either
import Test.Hspec
import Lex
import Parse

-- Don't run test sentences containing these words.
bannedWords :: [Text]
bannedWords =
  [ "about"
  , "cu"
  , "«Hello»"
  , "«I"
  , "keoru"
  , "re"
  , "should"
  , "the"
  ]

main :: IO ()
main = do
  sentences <- T.lines <$> T.readFile "test/sentences.txt"
  hspec (spec sentences)

spec :: [Text] -> Spec
spec sentences = do
  describe "Parse" $ do
    let lexOpt = LexOptions False
    let parseLex text = parseDiscourse =<< lexToaqOpt lexOpt text
    it "parses a simple sentence" $ do
      parseLex "Tủa jí jâı nháo da." `shouldSatisfy` isRight
    it "rejects a simple non-sentence" $ do
      parseLex "Tüa jí jâı nhào da." `shouldSatisfy` isLeft
    describe "A sentences" $ do
      forM_ sentences $ \sentence -> do
        when (all (`notElem` T.words sentence) bannedWords) $ do
          it (T.unpack sentence) $ do
            parseLex sentence `shouldSatisfy` isRight
