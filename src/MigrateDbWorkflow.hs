module MigrateDbWorkflow (migrateDbWorkflow) where

import           Control.Monad.Logger           (LoggingT, runStdoutLoggingT)
import           Database.Persist.Postgresql    (SqlPersistT,
                                                 withPostgresqlConn, ConnectionString)
import           Database.Persist.Sql.Migration (runMigration)
import           PsqlPersistence                (migrateAll)
import Control.Monad.Trans.Reader (ask, ReaderT (runReaderT))
import Control.Monad.Trans (lift)

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connString action = runStdoutLoggingT $ withPostgresqlConn connString $ \backend -> runReaderT action backend

migrateDbWorkflow :: ReaderT ConnectionString IO ()
migrateDbWorkflow = do 
    connString <- ask
    lift $ runAction connString (runMigration migrateAll)

