module Core (Address (..), EstateParameters (..), Offer (..), OfferSource (..), OfferType (..), GeoCoordinates (..)) where
import Data.Map (Map)
import Data.Time (UTCTime)

data GeoCoordinates = GeoCoordinates { getLng :: Double, getLat :: Double } deriving (Show, Eq)

data Address = Address {getCoordinates :: Maybe GeoCoordinates, getStreet :: Maybe String, getDistrict :: Maybe String, getCity :: Maybe String} deriving (Eq, Show)

data EstateParameters = EstateParameters{ getArea :: Maybe Int, getRooms :: Maybe Int, getFloor :: Maybe Int, getYearBuild :: Maybe Int, getBuildingType :: Maybe String, getTotalFloors :: Maybe Int, getRefNumber :: Maybe String, getParkingSpots :: Maybe Int, getOtherParams :: Map String String } deriving (Eq, Show)

data OfferSource = Gratka deriving (Eq, Show)
data OfferType = Rent | Sell deriving (Eq, Show)

data Offer = Offer{ getUrl :: String, getScrapeTime :: UTCTime, getSource :: OfferSource, getType :: OfferType, getAddress :: Address, getParameters :: EstateParameters, getTitle :: String, getPrice :: Maybe Int, getDescription :: String } deriving (Eq, Show)

