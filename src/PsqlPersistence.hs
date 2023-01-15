module PsqlPersistence (offerToDto, migrateAll, EstateDto (..), CommuteTimesViewDto (..), filterCommuteTime, updateCommuteTime) where

import qualified Database.Persist.TH as PTH
import Database.Persist.Postgresql ((==.), (=.), SqlPersistT)
import           Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Aeson
import Data.Time (UTCTime)
import Database.Persist (Filter, update, PersistEntity (Key))
import qualified Core
import Control.Monad.Logger (LoggingT)

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
    deriving Show Read
|]

PTH.share [PTH.mkPersist PTH.sqlSettings] [PTH.persistLowerCase|
  CommuteTimesViewDto sql=estates
    commuteTimeMins (Maybe Int)
    lng (Maybe Double)
    lat (Maybe Double)
    street (Maybe String)
    district (Maybe String)
    city (Maybe String)
    deriving Show Read
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
            (Core.getLng <$> Core.getCoordinates address)
            (Core.getLat <$> Core.getCoordinates address)
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

filterCommuteTime :: Maybe Int -> Filter CommuteTimesViewDto
filterCommuteTime x = CommuteTimesViewDtoCommuteTimeMins ==. x

updateCommuteTime :: Key CommuteTimesViewDto -> Int -> SqlPersistT (LoggingT IO) ()
updateCommuteTime estateId commuteTime = update estateId [CommuteTimesViewDtoCommuteTimeMins =. Just commuteTime]
