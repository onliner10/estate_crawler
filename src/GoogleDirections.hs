module GoogleDirections (getCommuteTime) where
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Core                       (GeoCoordinates (getLat, getLng))
import           Data.Time                  (NominalDiffTime, UTCTime)
import           Data.Time.Clock            (secondsToNominalDiffTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import qualified Google.Directions.Client   as GD
import qualified Google.Directions.Query    as GDQ
import           System.Posix.Types         (EpochTime)
import Control.Monad.Except (unless)

type SourceCoords = GeoCoordinates
type DestCoords = GeoCoordinates

formatCoordinates :: GeoCoordinates -> String
formatCoordinates c = show (Core.getLat c) ++ "," ++ show (Core.getLng c)

getShortestTransitTime :: GD.Directions -> Either String NominalDiffTime
getShortestTransitTime directions = do
    unless (GD.status directions == GD.OK) (Left $ "Invalid status code returned from API: " ++ show (GD.status directions))

    let durations = sum . fmap (GD.value . GD.legDuration) . GD.legs <$> GD.routes directions
    case durations of
      [] -> Left "No routes returned from Google Directions API"
      xs ->
        let shortest = minimum xs
        in Right $ secondsToNominalDiffTime $ realToFrac shortest

utcTimeToEpochTime :: UTCTime -> EpochTime
utcTimeToEpochTime = fromIntegral . toSecs
  where
    toSecs :: UTCTime -> Int
    toSecs = round . utcTimeToPOSIXSeconds

getCommuteTime :: GD.ApiKey -> SourceCoords -> DestCoords -> UTCTime -> IO (Either String NominalDiffTime)
getCommuteTime apiKey src dst arrivalTime = runExceptT $ do
    let q =
            GDQ.withTravelMode GDQ.Transit $
                GDQ.withArrivalTime (utcTimeToEpochTime arrivalTime) $
                    GDQ.createDirectionsQuery (formatCoordinates src) (formatCoordinates dst)

    response <- ExceptT $ GD.getDirections apiKey q
    ExceptT $ return $ getShortestTransitTime response

