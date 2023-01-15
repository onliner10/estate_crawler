module FillCommuteTimesWorkflow (fillCommuteTimesWorkflow, FillCommuteTimesWorkflowDeps (FillCommuteTimesWorkflowDeps)) where
import           Control.Monad.Cont          (MonadIO)
import           Control.Monad.Logger        (LoggingT, runStdoutLoggingT)
import           Control.Monad.Reader        (runReaderT)
import           Control.Monad.Trans         (lift)
import           Control.Monad.Trans.Reader  (ReaderT, ask)
import qualified Core
import           Database.Persist.Postgresql (ConnectionString, Entity (entityVal, entityKey),
                                              SqlPersistT, selectList,
                                              withPostgresqlConn)
import qualified Google.Directions.Client    as GDC
import qualified GoogleDirections            as GD
import qualified PsqlPersistence             as SQL
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (MaybeT))
import Control.Monad (void, MonadPlus (mzero))
import Core (GeoCoordinates(getLng))
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import PsqlPersistence (updateCommuteTime)

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connString action = runStdoutLoggingT $ withPostgresqlConn connString $ \backend -> runReaderT action backend

getEstatesWithNoCommuteTimes :: (MonadIO m) => SqlPersistT m [Entity SQL.CommuteTimesViewDto]
getEstatesWithNoCommuteTimes = selectList [SQL.filterCommuteTime Nothing] []

data FillCommuteTimesWorkflowDeps = FillCommuteTimesWorkflowDeps{getConnString :: ConnectionString, getDirectionsApiKey:: GDC.ApiKey}

hoistMaybe :: MonadPlus m => Maybe a -> MaybeT m a
hoistMaybe = maybe mzero return

fillCommuteTime :: FillCommuteTimesWorkflowDeps -> Entity SQL.CommuteTimesViewDto -> IO ()
fillCommuteTime deps srcEntity = void $ runMaybeT $ do
    srcLat <- hoistMaybe $ SQL.commuteTimesViewDtoLat $ entityVal srcEntity
    srcLng <- hoistMaybe $ SQL.commuteTimesViewDtoLng $ entityVal srcEntity
    
    let 
        expectedArrival = UTCTime (fromGregorian 2023 1 16) (secondsToDiffTime 3600) 
        sourceCords = Core.GeoCoordinates { getLat = srcLat, getLng = srcLng }
        dstCoords = Core.GeoCoordinates { getLat = 52.22877808042894, getLng = 20.98412501166858 }
        apiKey = getDirectionsApiKey deps

    commuteTime <- MaybeT $ either (const Nothing) Just <$> GD.getCommuteTime apiKey sourceCords dstCoords expectedArrival

    let 
        commuteTimeSeconds :: Int
        commuteTimeSeconds = round commuteTime
        commuteTimeMinutes = commuteTimeSeconds `div` 60
        updateAction = updateCommuteTime (entityKey srcEntity) commuteTimeMinutes
        connString = getConnString deps

    lift $ runAction connString updateAction

fillCommuteTimesWorkflow :: ReaderT FillCommuteTimesWorkflowDeps IO ()
fillCommuteTimesWorkflow = do
    deps <- ask
    estates <- lift $ runAction (getConnString deps) getEstatesWithNoCommuteTimes

    let updateActions = fillCommuteTime deps <$> estates
    lift $ sequence_ updateActions

    lift $ putStrLn "Fill commute action done!"
