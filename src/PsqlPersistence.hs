module PsqlPersistence (offerToDto, migrateAll, EstateDto (..), CommuteTimesViewDto (..), filterCommuteTime, updateCommuteTime, getCachedCommuteTime) where

import qualified Database.Persist.TH as PTH
import           Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Aeson
import Data.Time (UTCTime, NominalDiffTime, secondsToNominalDiffTime)
import qualified Core
import Data.LatLong as Geo
import Control.Monad.Cont
import Database.Esqueleto.Experimental
import Data.Morton (Interval(Interval))
import qualified Data.LatLong as Get
import Control.Error (headMay, fromMaybe)
import Data.List (sort)
import GHC.OldList (sortOn)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  EstateDto sql=estates
    url String
    commuteTimeMins (Maybe Int)
    scrapeDate UTCTime
    offerSource String
    offerType String
    title String
    price (Maybe Int)
    description String
    lng (Maybe Double)
    lat (Maybe Double)
    latLng (Maybe Geo.LatLong)
    street (Maybe String)
    district (Maybe String)
    city (Maybe String)
    area (Maybe Int)
    rooms (Maybe Int)
    floor (Maybe Int)
    yearBuilt (Maybe Int)
    buildingType (Maybe String)
    totalFloors (Maybe Int)
    refNumber (Maybe String)
    parkingSpots (Maybe Int)
    otherParams String
    deriving Show 
|]

PTH.share [PTH.mkPersist PTH.sqlSettings] [PTH.persistLowerCase|
  CommuteTimesViewDto sql=estates
    commuteTimeMins (Maybe Int)
    lng (Maybe Double)
    lat (Maybe Double)
    latLng (Maybe Geo.LatLong)
    street (Maybe String)
    district (Maybe String)
    city (Maybe String)
    deriving Show
|]

offerSourceToString :: Core.OfferSource -> String
offerSourceToString Core.Gratka = "gratka.pl"

offerTypeToString :: Core.OfferType -> String
offerTypeToString Core.Rent = "rent"
offerTypeToString Core.Sell = "sell"

offerToDto :: Core.Offer -> EstateDto
offerToDto o =
    let params = Core.getParameters o
        address = Core.getAddress o
        parsedLng = (Core.getLng <$> Core.getCoordinates address)
        parsedLat = (Core.getLat <$> Core.getCoordinates address)
    in
        EstateDto 
            (Core.getUrl o)
            Nothing
            (Core.getScrapeTime o)
            (offerSourceToString $ Core.getSource o)
            (offerTypeToString $ Core.getType o)
            (Core.getTitle o) 
            (Core.getPrice o)
            (Core.getDescription o)
            parsedLng
            parsedLat
            (Geo.LatLong <$> parsedLat <*> parsedLng)
            (Core.getStreet address)
            (Core.getDistrict address)
            (Core.getCity address)
            (Core.getArea params)
            (Core.getRooms params)
            (Core.getFloor params)
            (Core.getYearBuild params)
            (Core.getBuildingType params)
            (Core.getTotalFloors params)
            (Core.getRefNumber params)
            (Core.getParkingSpots params)
            (BLU.toString $ Data.Aeson.encode $ Core.getOtherParams params)

filterCommuteTime :: (MonadIO m) => SqlPersistT m [Entity CommuteTimesViewDto]
filterCommuteTime = 
    select $ do
    row <- from $ table @CommuteTimesViewDto
    where_ $ isNothing (row ^. CommuteTimesViewDtoCommuteTimeMins)
    pure row

updateCommuteTime :: (MonadIO m) => Key CommuteTimesViewDto -> Int -> SqlPersistT m ()
updateCommuteTime estateId commuteTime = 
    update $ \r -> do
        set r [ CommuteTimesViewDtoCommuteTimeMins =. just (val commuteTime)]
        where_ $ r ^. CommuteTimesViewDtoId ==. val estateId

withinTileSet' :: [Get.LatLongTile] -> SqlExpr (Value Geo.LatLong) -> SqlExpr (Value Bool)
withinTileSet' tiles field = let
    tfilter tile = let Interval a b = latLongTileInterval tile in (field >=. val a) &&. (field <=. val b)
    in Prelude.foldl (||.) (val False) $ fmap tfilter tiles

getCachedCommuteTime :: (MonadIO m) => Double -> Core.GeoCoordinates -> SqlPersistT m (Maybe NominalDiffTime)
getCachedCommuteTime radius c = 
    let
        parsedLng = Core.getLng c
        parsedLat = Core.getLat c
        zeroCoord = Geo.LatLong 0 0
        latLng = Geo.LatLong parsedLat parsedLng
        tiles = latLongTileCoverSquare latLng radius
        queryResult =  
            select $ do
            row <- from $ table @CommuteTimesViewDto
            let 
                srcCoords :: SqlExpr (Value Geo.LatLong)
                srcCoords = coalesceDefault [row ^. CommuteTimesViewDtoLatLng] (val zeroCoord)
            where_ $ withinTileSet' tiles srcCoords
            limit 100
            return row
    in do
        nearby <- queryResult
        let
            xs = fmap entityVal nearby
            calculateDistance x = Geo.geoDistance latLng $ fromMaybe zeroCoord (commuteTimesViewDtoLatLng x)
            minDistance = headMay $ sortOn calculateDistance xs
            resultTime = secondsToNominalDiffTime . (*) 60 . realToFrac <$> (minDistance >>= commuteTimesViewDtoCommuteTimeMins)

        return resultTime 
