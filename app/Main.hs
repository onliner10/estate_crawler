module Main (main) where

import           Control.Monad.Trans.Reader  (ReaderT, runReaderT)
import           Data.ByteString.UTF8        as BSU
import qualified Data.Text                   as T
import           Database.Persist.Postgresql (ConnectionString)
import           FillCommuteTimesWorkflow    (FillCommuteTimesWorkflowDeps (FillCommuteTimesWorkflowDeps),
                                              fillCommuteTimesWorkflow)
import qualified Google.Directions.Client    as GDC
import           MigrateDbWorkflow           (migrateDbWorkflow)
import           ScrapeGratkaWorkflow        (scrapeGratkaWorkflow)
import           System.Environment          (getArgs, getEnv)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT, logWithoutLoc)
import Control.Monad.Trans.Class (lift)

getConnString :: IO ConnectionString
getConnString = BSU.fromString <$> getEnv "CONNECTION_STRING"

getDirectionsApiKey :: IO GDC.ApiKey
getDirectionsApiKey = GDC.ApiKey . T.pack <$> getEnv "GDIRECTIONS_API_KEY"

withSqlConnection :: ReaderT ConnectionString IO () -> IO ()
withSqlConnection action = do
    connString <- getConnString

    runReaderT action connString

withCommuteDeps :: ReaderT FillCommuteTimesWorkflowDeps (LoggingT IO) () -> LoggingT IO ()
withCommuteDeps action = do
    connString <- lift getConnString
    apiKey <- lift getDirectionsApiKey
    let deps = FillCommuteTimesWorkflowDeps connString apiKey

    runReaderT action deps

runLogger :: LoggingT IO () -> IO ()
runLogger = runStdoutLoggingT

handleCommand :: String -> IO ()
handleCommand "scrape-gratka" = withSqlConnection scrapeGratkaWorkflow
handleCommand "fill-commute"  = runLogger $ withCommuteDeps fillCommuteTimesWorkflow
handleCommand "migrate-db"    = withSqlConnection migrateDbWorkflow
handleCommand x               = putStrLn $ "Unknown command: " ++ x

main :: IO ()
main = do
    args <- getArgs

    mapM_ handleCommand args

    putStrLn "Done"
