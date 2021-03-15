{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Dictionary
import DictionaryIO
import Options.Applicative

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (configParse <**> helper)
        ( fullDesc
            <> progDesc "generate a dictionary file from text-pronunciation pairs"
            <> header "mk-dict - make a dictionary"
        )

run :: Config -> IO ()
run config = do
  putStrLn "NOTE: currently only using the first 500 entries for testing"
  putStrLn ("generating dictionary from: " <> pairsFile config)
  putStrLn ("output file is: " <> outFile config)
  pairs <- fmap pairFromLine . T.lines <$> TIO.readFile (pairsFile config)
  putStrLn ("number of pairs (unfiltered): " <> show (length pairs))
  let pairsFiltered = filter (\(ortho, pron) -> (ortho /= "") && (pron /= "")) pairs
      pairsTEMP = take 500 pairsFiltered -- something manageable for testing
  putStrLn ("number of pairs: " <> show (length pairsFiltered))
  pairs' <- case defsFile config of
    Nothing -> return $ (\(ortho, pron) -> (ortho, [], pron)) <$> pairsTEMP
    Just f -> addDefs f pairsTEMP
  let dict = fromList $ uncurry3 makeEntry <$> pairs'
  putStrLn ("number of entries: " <> show (size dict))
  writeDictionary (outFile config) dict

pairFromLine :: T.Text -> (T.Text, T.Text)
pairFromLine l =
  let (ortho, pron) = T.breakOn "\t" l
   in (T.strip ortho, T.strip pron)

type PreEntry = (T.Text, [(T.Text, T.Text, [T.Text])], T.Text)

addDefs :: FilePath -> [(T.Text, T.Text)] -> IO [PreEntry]
addDefs = undefined

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

data Config = Config
  { pairsFile :: FilePath,
    defsFile :: Maybe FilePath,
    outFile :: FilePath,
    shouldReport :: Bool
  }

configParse :: Parser Config
configParse =
  Config
    <$> argument
      str
      ( metavar "PAIRS_FILE"
          <> help "a file with text-pronunciation pairs"
      )
    <*> optional
      ( strOption
          ( long "definitions"
              <> short 'd'
              <> metavar "DEFS_FILE"
              <> help "a file with definition info: text, pron, pos, gloss, [tags]"
          )
      )
    <*> strOption
      ( long "out"
          <> short 'o'
          <> metavar "OUT.jsonl"
          <> help "name for the generated dictionary file"
          <> showDefault
          <> value "dict.jsonl"
      )
    <*> switch
      ( long "report"
          <> short 'r'
          <> help "report info about dictionary after construction"
      )
