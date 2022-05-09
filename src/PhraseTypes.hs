{-# LANGUAGE RecordWildCards #-}

module PhraseTypes where

import Data.Char
import Data.List.Extra
import qualified Data.Map.Strict as Map
import Data.Maybe
import System.Random (RandomGen, randomR)

data PhraseConfig = PhraseConfig
  { phrasePattern :: String,
    maxWordLength :: Maybe Int,
    numPassPhrases :: Maybe Int,
    englishDictFile :: FilePath,
    spanishDictFile :: FilePath
  }

data PhraseEnv = PhraseEnv
  { dicts :: IO (Map.Map Char [String]),
    numToGen :: Int,
    wordFilter :: Maybe (String -> Bool),
    pattern :: String
  }

numbers :: [String]
numbers = map show [0 .. 9]

-- We do not use show here, because it adds the single quotes to each character, we just convert to [Char] e.g. String
symbols :: [String]
symbols = map (: []) $ filter (\c -> (isSymbol c || isPunctuation c) && isAscii c) $ enumFrom minBound

fileToDict :: Char -> FilePath -> Maybe (String -> Bool) -> IO (Char, [String])
fileToDict c fpth wordF = (\content -> (c, caseWords . filterWords $ lines content)) <$> readFile fpth
  where
    caseWords = map (\(c : cs) -> toUpper c : lower cs)
    filterWords words = maybe words (`filter` words) wordF

buildDicts :: String -> String -> Maybe (String -> Bool) -> IO (Map.Map Char [String])
buildDicts engFp spaFp wordF = Map.fromList <$> sequence [return ('#', numbers), return ('*', symbols), fileToDict 'E' engFp wordF, fileToDict 'S' spaFp wordF]

toPhraseEnv :: PhraseConfig -> PhraseEnv
toPhraseEnv config@PhraseConfig {..} =
  PhraseEnv
    { pattern = phrasePattern,
      numToGen = fromMaybe 1 numPassPhrases,
      wordFilter = wordF,
      dicts = buildDicts englishDictFile spanishDictFile wordF
    }
  where
    wordF = (\l -> (<= l) . length) <$> maxWordLength
