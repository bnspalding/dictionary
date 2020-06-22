module Main where

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
run config = putStrLn ("pairsFile: " <> pairsFile config)

data Config
  = Config
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
