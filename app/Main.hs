{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import qualified Data.Text.IO as TIO
import Options.Applicative
import PassPhraseApp
import PhraseTypes
import System.Random (newStdGen)

passwordGenSetup :: Parser PhraseConfig
passwordGenSetup =
  PhraseConfig
    <$> strOption (long "pattern" <> short 'p' <> metavar "PATTERN" <> help "Passphrase generation pattern")
    <*> optional (option auto (long "max-length" <> short 'm' <> metavar "MAX-WORD-LENGTH" <> help "Maximum word word length"))
    <*> optional (option auto (long "num-passphrases" <> short 'n' <> metavar "NUM_PASSPHRASES" <> help "Number of passphrases to generate"))
    <*> strOption (long "english-dict" <> short 'e' <> metavar "ENGLISH-DICT-FILE" <> help "File containing English words")
    <*> strOption (long "spanish-dict" <> short 's' <> metavar "SPANISH-DICT-FILE" <> help "File containing Spanish words")

main :: IO ()
main = execParser opts >>= printRandomPassPhrases
  where
    opts =
      info
        (passwordGenSetup <**> helper)
        ( fullDesc
            <> progDesc "Generate candidate pass phrases"
            <> header "PasswordGen - My Passphrase generator"
        )

printRandomPassPhrases :: PhraseConfig -> IO ()
printRandomPassPhrases phrcfg = void (outputRandomPassphrasesUntil (toPhraseEnv phrcfg))

dump :: PhraseConfig -> IO ()
dump = dumpEnv . toPhraseEnv
  where
    dumpEnv pe = do
      putStrLn $ "Pattern: " <> pattern pe
      dcts <- dicts pe
      putStrLn $ "Dicts: " <> show dcts
      rg <- newStdGen
      (phr1, rg2) <- randomWord pe 'E' rg
      TIO.putStrLn $ "English rando word " <> phr1
      (phr2, rg3) <- randomWord pe 'S' rg2
      TIO.putStrLn $ "Spanish rando word " <> phr2
      (phr3, rg4) <- randomWord pe '#' rg3
      TIO.putStrLn $ "Number rando word " <> phr3
      (phr4, rg5) <- randomWord pe '*' rg4
      TIO.putStrLn $ "Symbol rando word " <> phr4
