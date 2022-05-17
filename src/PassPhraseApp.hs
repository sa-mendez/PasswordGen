{-# LANGUAGE OverloadedStrings #-}

module PassPhraseApp
  ( outputRandomPassphrasesUntil,
    outputRandomPassphrases,
    outputSingleRandomPassphrase,
    randomWord,
  )
where

import Control.Applicative (empty)
import Control.Monad.Reader
  ( MonadIO (liftIO),
    MonadPlus (mzero),
    MonadReader (ask),
    MonadTrans (lift),
    ReaderT (runReaderT),
    asks,
    forever,
    when,
  )
import Control.Monad.State
  ( MonadState (get, put),
    StateT,
    evalStateT,
  )
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Writer.CPS
  ( MonadIO (liftIO),
    MonadPlus (mzero),
    MonadTrans (lift),
    MonadWriter (tell),
    WriterT,
    censor,
    forever,
    mapWriterT,
    runWriterT,
    when,
  )
import Data.Map.Strict ((!?))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Tuple (swap)
import PhraseTypes (PhraseEnv (dicts, numToGen, pattern))
import System.IO (hFlush, stdout)
import System.Random
  ( Random (randomR),
    RandomGen,
    StdGen,
    newStdGen,
  )

{-
 Notice the WriterT instance here.  It is actually from Control.Monad.Writer.CPS, not the Strict or Lazy Writer
 This is because (and it took me a while to get to the bottom of this), the Strict and Lazy Writers are known to
 have space leaks.  I am of course using the Writer to build up the pass phrases, and as you scale up the number
 of generated pass phrases (via the -n parameter) the memory usage skyrockets.

 Note that I also had to add the writer-cps-mtl package as a dependency, as it contains the MonadState, MonadReader
 orphan instances for the CPS Writer
-}
type PassPhraseApp g r = WriterT T.Text (ReaderT PhraseEnv (StateT g IO)) r

outputRandomPassphrasesUntil :: PhraseEnv -> IO (Maybe ())
outputRandomPassphrasesUntil phrenv = do
  rg <- newStdGen
  let untilLoop = runMaybeT $
        forever $ do
          liftIO (putStrLn $ "\n\nGenerating " <> show (numToGen phrenv) <> " passphrases\n")
          passPhrases <- lift $ mapM (const randomPassPhraseAndClear) [1 .. (numToGen phrenv)]
          liftIO (TIO.putStrLn $ T.unlines passPhrases)
          liftIO $ putStr "Generate more passphrases (y/n): " >> hFlush stdout
          resp <- liftIO getLine
          when (resp == "n") mzero
  runPassPhraseApp phrenv rg untilLoop

outputRandomPassphrases :: PhraseEnv -> IO ()
outputRandomPassphrases phrenv = do
  rg <- newStdGen
  putStrLn $ "Generating " <> show (numToGen phrenv) <> " passphrases"
  passPhrases <- runPassPhraseApp phrenv rg $ mapM (const randomPassPhraseAndClear) [1 .. (numToGen phrenv)]
  TIO.putStrLn $ T.unlines passPhrases

outputSingleRandomPassphrase :: PhraseEnv -> IO ()
outputSingleRandomPassphrase phrenv = do
  rg <- newStdGen
  passPhrase <- runPassPhraseApp phrenv rg randomPassPhrase
  TIO.putStrLn $ "The randomly generated pass phrase is " <> passPhrase

runPassPhraseApp :: PhraseEnv -> StdGen -> PassPhraseApp StdGen r -> IO r
runPassPhraseApp phrenv rg app = do
  fst <$> evalStateT (runReaderT (runWriterT app) phrenv) rg

-- Diagnosing why using censor here was yielding blank results was a tricky issue to diagnose
-- It basically had to do with the fact that I was using execWriterT instead of runWriterT
-- in the runPassPhraseApp.
randomPassPhraseAndClear :: PassPhraseApp StdGen T.Text
randomPassPhraseAndClear = censor (const mempty) randomPassPhrase

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
    dictRandoItem rg dictEntries = swap $ (dictEntries !!) <$> swap (randomR (0, length dictEntries - 1) rg)
    dictOrUnknown dictKey dicts = fromMaybe ["?"] (dicts !? dictKey)
