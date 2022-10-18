module Main (main)
    where

--------------------------------------------------------------------------------
import           Prelude
import           Data.Text
--------------------------------------------------------------------------------
import           Control.Exception.Safe
import           Data.Maybe             (fromMaybe)
import           Options.Applicative    as Opt
import           System.Exit
import           System.IO.Error        (ioeGetFileName, isDoesNotExistError)
--------------------------------------------------------------------------------
import qualified App
import           AppTypes
--------------------------------------------------------------------------------
--
-- MOUNT_POINT + '/diamonds/{project_id}/warehouse.sqlite',
-- space + '{project_id}/shared/diamonds/{project_id}/warehouse.sqlite',
--
mkConfig :: Opt.Parser (FileShareCfg -> Config)
mkConfig = Config
  <$> option auto (metavar "PORT" <> short 'p' <> long "port"
                 <> value 5003 <> showDefault <> help "Http port number ")

  <*> strOption (metavar "MOUNT_POINT" <> short 'm' <> long "mount"
                 <> value "/shared" <> showDefault <> help "Shared drive mount point")

  <*> strOption (metavar "DATA_DIR" <> short 'd' <> long "data"
                 <> value "" <> showDefault <> help "Data directory")


withConfig :: (FileShareCfg -> Config) -> IO ()
withConfig cfgWithoutShare = do
    cfg <- fmap cfgWithoutShare fileShareCfgFromEnv
    putStrLn ("🟢 listening on port: " <> show(port cfg) <> "...")
    putStrLn ("mount/data dir: " <> unpack(mountPoint cfg) <> "/" <> unpack(dataDir cfg))
    putStrLn ("file share uri: " <> unpack(hostBucket(fileShareCfg cfg)))
    App.exec cfg


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



--------------------------------------------------------------------------------
