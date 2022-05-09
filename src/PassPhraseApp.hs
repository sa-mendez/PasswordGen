{-# LANGUAGE OverloadedStrings #-}

module PassPhraseApp where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map.Strict ((!?))
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Tuple (swap)
import PhraseTypes
import System.Random

type PassPhraseApp g r = WriterT T.Text (ReaderT PhraseEnv (StateT g IO)) r

outputRandomPassphrases :: PhraseEnv -> IO ()
outputRandomPassphrases phrenv = do
  putStrLn $ "Generating " <> show (numToGen phrenv) <> " passphrases"
  passPhrases <- runPassPhraseApp phrenv $ mapM (const randomPassPhraseAndClear) [1 .. (numToGen phrenv)]
  TIO.putStrLn $ T.unlines passPhrases
  where
    -- Diagnosing why using censor here was yielding blank results was a tricky issue to diagnose
    -- It basically had to do with the fact that I was using execWriterT instead of runWriterT
    -- in the runPassPhraseApp.
    randomPassPhraseAndClear :: PassPhraseApp StdGen T.Text
    randomPassPhraseAndClear = censor (const mempty) randomPassPhrase

outputSingleRandomPassphrase :: PhraseEnv -> IO ()
outputSingleRandomPassphrase phrenv = do
  passPhrase <- runPassPhraseApp phrenv randomPassPhrase
  TIO.putStrLn $ "The randomly generated pass phrase is " <> passPhrase

runPassPhraseApp :: PhraseEnv -> PassPhraseApp StdGen r -> IO r
runPassPhraseApp phrenv app = do
  rg <- newStdGen
  fst <$> evalStateT (runReaderT (runWriterT app) phrenv) rg

randomPassPhrase :: RandomGen g => PassPhraseApp g T.Text
randomPassPhrase = do
  pat <- asks pattern
  rg <- get
  mapWriterT (fmap writerToResult) $ mapM_ rndWord pat
  where
    rndWord :: RandomGen g => Char -> PassPhraseApp g ()
    rndWord dictKey = do
      -- (word, rgn) <- liftM3 randomWord ask (return dictKey) get
      phrenv <- ask
      rg <- get
      (word, rgn) <- liftIO $ randomWord phrenv dictKey rg
      put rgn
      tell word
    writerToResult (_, w) = (w, w)

randomWord :: RandomGen g => PhraseEnv -> Char -> g -> IO (T.Text, g)
randomWord penv dictKey rg = do
  dicts <- dicts penv
  return (dictRandoItem rg (dictOrUnknown dictKey dicts))
  where
    dictRandoItem rg dictEntries = swap $ T.pack . (dictEntries !!) <$> swap (randomR (0, length dictEntries - 1) rg)
    dictOrUnknown dictKey dicts = fromMaybe ["?"] (dicts !? dictKey)
