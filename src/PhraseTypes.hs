{-# LANGUAGE RecordWildCards #-}

module PhraseTypes where

import Data.Char (intToDigit, isAscii, isPunctuation, isSymbol)
import Data.List.Extra ()
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.IO (hGetContents)
import GHC.IO.IOMode (IOMode (ReadMode))
import System.IO (withFile)
import System.Random (RandomGen, randomR)

data PhraseConfig = PhraseConfig
  { phrasePattern :: String,
    maxWordLength :: Maybe Int,
    numPassPhrases :: Maybe Int,
    englishDictFile :: FilePath,
    spanishDictFile :: FilePath
  }

data PhraseEnv = PhraseEnv
  { dicts :: IO (Map.Map Char [T.Text]),
    numToGen :: Int,
    wordFilter :: Maybe (T.Text -> Bool),
    pattern :: String
  }

toPhraseEnv :: PhraseConfig -> PhraseEnv
toPhraseEnv config@PhraseConfig {..} =
  PhraseEnv
    { pattern = phrasePattern,
      numToGen = fromMaybe 1 numPassPhrases,
      wordFilter = wordF,
      dicts = buildDicts englishDictFile spanishDictFile wordF
    }
  where
    wordF = (\l -> (<= l) . T.length) <$> maxWordLength

buildDicts :: String -> String -> Maybe (T.Text -> Bool) -> IO (Map.Map Char [T.Text])
buildDicts engFp spaFp wordF = Map.fromList <$> sequence [return ('#', numbers), return ('*', symbols), fileToDict 'E' engFp wordF, fileToDict 'S' spaFp wordF]

fileToDict :: Char -> FilePath -> Maybe (T.Text -> Bool) -> IO (Char, [T.Text])
fileToDict c fpth wordF = (\textLines -> (c, caseWords . filterWords $ textLines)) <$> withFile fpth ReadMode readFileLines
  where
    caseWords = map T.toTitle
    filterWords words = maybe words (`filter` words) wordF
    readFileLines fh = T.lines <$> hGetContents fh

numbers :: [T.Text]
numbers = map (T.singleton . intToDigit) [0 .. 9]

-- We do not use show here, because it adds the single quotes to each character, we just convert to [Char] e.g. String
symbols :: [T.Text]
symbols = map T.singleton $ filter (\c -> (isSymbol c || isPunctuation c) && isAscii c) $ enumFrom minBound
