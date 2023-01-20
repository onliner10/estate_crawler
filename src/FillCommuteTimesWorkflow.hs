module FillCommuteTimesWorkflow (fillCommuteTimesWorkflow, FillCommuteTimesWorkflowDeps (FillCommuteTimesWorkflowDeps)) where
import           Control.Monad.Cont          (MonadIO)
import           Control.Monad.Logger        (LoggingT, LogLevel (LevelInfo, LevelError), logWithoutLoc, LogSource)
import           Control.Monad.Reader        (runReaderT)
import           Control.Monad.Trans         (lift)
import           Control.Monad.Trans.Reader  (ReaderT, ask)
import qualified Core
import           Database.Persist.Postgresql (ConnectionString, Entity (entityVal, entityKey),
                                              SqlPersistT,
                                              withPostgresqlConn)
import qualified Google.Directions.Client    as GDC
import qualified GoogleDirections            as GD
import qualified PsqlPersistence             as SQL
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (MaybeT))
import Control.Monad (void, MonadPlus (mzero))
import Core (GeoCoordinates(getLng))
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import PsqlPersistence (updateCommuteTime, filterCommuteTime, getCachedCommuteTime)

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> LoggingT IO a
runAction connString action = withPostgresqlConn connString $ \backend -> runReaderT action backend

getEstatesWithNoCommuteTimes :: (MonadIO m) => SqlPersistT m [Entity SQL.CommuteTimesViewDto]
getEstatesWithNoCommuteTimes = filterCommuteTime 

data FillCommuteTimesWorkflowDeps = FillCommuteTimesWorkflowDeps{getConnString :: ConnectionString, getDirectionsApiKey:: GDC.ApiKey}

hoistMaybe' :: Maybe a -> MaybeT (LoggingT IO) a
hoistMaybe' = maybe mzero return

logEither :: LogSource -> IO (Either String a) -> LoggingT IO (Maybe a)
logEither src x =
    let logErr :: String -> LoggingT IO (Maybe a)
        logErr e = Nothing <$ logWithoutLoc src LevelError e
     in lift x >>= either logErr (return . Just)

fillCommuteTime :: FillCommuteTimesWorkflowDeps -> Entity SQL.CommuteTimesViewDto -> LoggingT IO ()
fillCommuteTime deps srcEntity = void $ runMaybeT $ do
    srcLat <- hoistMaybe' $ SQL.commuteTimesViewDtoLat $ entityVal srcEntity
    srcLng <- hoistMaybe' $ SQL.commuteTimesViewDtoLng $ entityVal srcEntity
    
    let 
        expectedArrival = UTCTime (fromGregorian 2023 1 16) (secondsToDiffTime 8 * 3600) 
        sourceCords = Core.GeoCoordinates { getLat = srcLat, getLng = srcLng }
        dstCoords = Core.GeoCoordinates { getLat = 52.22877808042894, getLng = 20.98412501166858 }
        apiKey = getDirectionsApiKey deps
        connString = getConnString deps

    logWithoutLoc "FILL-COMMUTE" LevelInfo ("Searching route from:" ++ show sourceCords)

    let getDirectionsFromApi = MaybeT $  logEither "FILL-COMMUTE" $ GD.getCommuteTime apiKey sourceCords dstCoords expectedArrival
    let logGetFromCache x = logWithoutLoc "FILL-COMMUTE" LevelInfo ("Getting cached value for: " ++ show sourceCords ++ ": " ++ show x)
    cachedCommute <- lift $ runAction connString $ getCachedCommuteTime 100 sourceCords

    commuteTime <- maybe getDirectionsFromApi (\x -> logGetFromCache x >> return x) cachedCommute

    let 
        commuteTimeSeconds :: Int
        commuteTimeSeconds = round commuteTime
        commuteTimeMinutes = commuteTimeSeconds `div` 60
        updateAction = updateCommuteTime (entityKey srcEntity) commuteTimeMinutes

    lift $ runAction connString updateAction

fillCommuteTimesWorkflow :: ReaderT FillCommuteTimesWorkflowDeps (LoggingT IO) ()
fillCommuteTimesWorkflow = do
    deps <- ask
    logWithoutLoc "FILL-COMMUTE" LevelInfo ("Starting workflow" :: String)
    estates <- lift $ runAction (getConnString deps) getEstatesWithNoCommuteTimes

    let updateActions = fillCommuteTime deps <$> estates
    lift $ sequence_ updateActions

    logWithoutLoc "FILL-COMMUTE" LevelInfo ("Finished workflow" :: String)
