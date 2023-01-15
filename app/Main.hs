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

getConnString :: IO ConnectionString
getConnString = BSU.fromString <$> getEnv "CONNECTION_STRING"

getDirectionsApiKey :: IO GDC.ApiKey
getDirectionsApiKey = GDC.ApiKey . T.pack <$> getEnv "GDIRECTIONS_API_KEY"

withSqlConnection :: ReaderT ConnectionString IO () -> IO ()
withSqlConnection action = do
    connString <- getConnString

    runReaderT action connString

withCommuteDeps :: ReaderT FillCommuteTimesWorkflowDeps IO () -> IO ()
withCommuteDeps action = do
    connString <- getConnString
    apiKey <- getDirectionsApiKey
    let deps = FillCommuteTimesWorkflowDeps connString apiKey

    runReaderT action deps

handleCommand :: String -> IO ()
handleCommand "scrape-gratka" = withSqlConnection scrapeGratkaWorkflow
handleCommand "fill-commute"  = withCommuteDeps fillCommuteTimesWorkflow
handleCommand "migrate-db"    = withSqlConnection migrateDbWorkflow
handleCommand x               = putStrLn $ "Unknown command: " ++ x

main :: IO ()
main = do
    putStrLn "Starting"
    args <- getArgs

    mapM_ handleCommand args

    putStrLn "Done"
