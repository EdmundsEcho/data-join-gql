module Main where

import           Control.Exception.Safe
import           Data.Maybe             (fromMaybe)
import           Options.Applicative    as Opt
import           Prelude
import           System.Exit
import           System.IO.Error        (ioeGetFileName, isDoesNotExistError)

import qualified App
import           Types


mkConfig :: Opt.Parser AppConfig
mkConfig = AppConfig
  <$> option auto (metavar "PORT" <> short 'p' <> long "port"
                 <> value 5003 <> showDefault <> help "Http port number ")

withConfig :: AppConfig -> IO ()
withConfig = App.exec

main :: IO ()
main =
  (execParser opts >>= withConfig)
    `catches` [ Handler parserExit,
                Handler printIOError,
                Handler decodeConfigError,
                Handler printOtherErrors
              ]
  where
    opts =
      info
        (mkConfig <**> helper)
        ( fullDesc
            <> progDesc "Graphql ETL server of statistical data"
        )
    parserExit :: ExitCode -> IO ()
    parserExit _ = pure ()
    printIOError :: IOException -> IO ()
    printIOError e
      | isDoesNotExistError e = do
        let mbfn = ioeGetFileName e
        putStrLn $ "File " <> fromMaybe "" mbfn <> " not found"
      | otherwise = putStrLn $ "I/O error: " <> show e

    decodeConfigError :: StringException -> IO ()
    decodeConfigError (StringException s _) = putStrLn s

    printOtherErrors :: SomeException -> IO ()
    printOtherErrors = print
