module GratkaScraper (scrapeOffer, scrapeListingPage, ListingPage (getOfferLinks, getNextPage)) where

import           Control.Applicative       (optional)
import           Control.Error             (atErr, headErr, headMay)
import           Control.Monad             (MonadPlus (mzero))
import           Control.Monad.State       (State, evalState, get, put)
import           Control.Monad.State.Lazy  (gets)
import           Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import           Data.Aeson                as JSON
import           Data.Aeson.Types          as JSONT
import           Data.ByteString.Lazy.UTF8 as BLU
import           Data.Map.Lazy             (Map, delete, fromList, lookup)
import           Prelude                   hiding (lookup)
import           Text.HTML.Scalpel         (Scraper, URL, attr, attrs, chroots,
                                            hasClass, scrapeURL, text, texts,
                                            (//), (@:))
import           Text.Regex.Posix          ((=~))
import Data.Time.Clock (getCurrentTime)
import qualified Core

-- TODO: Use bytestrings everywhere!

data ListingPage = ListingPage { getNextPage :: Maybe URL, getOfferLinks :: [URL] } deriving (Eq, Show)

scrapeListingPage :: URL -> IO (Maybe ListingPage)
scrapeListingPage url = scrapeURL url scraper
    where
        teaserLinkS = "div" @: [hasClass "listing__teaserWrapper"] // "a" @: [hasClass "teaserLink"]
        links :: Scraper String [URL]
        links = attrs "href" teaserLinkS
        nextPageS = "div" @: [hasClass "pagination"] // "a" @: [hasClass "pagination__nextPage"]
        nextPage :: Scraper String (Maybe URL)
        nextPage = optional (attr "href" nextPageS)
        scraper = ListingPage <$> nextPage <*> links

geoCoordinatesParser :: JSON.Object -> JSONT.Parser Core.GeoCoordinates
geoCoordinatesParser obj = do
    lng <- obj .: "lokalizacja_dlugosc-geograficzna-x"
    lat <- obj .: "lokalizacja_szerokosc-geograficzna-y"

    return $ Core.GeoCoordinates lng lat

addressParser :: JSON.Value -> JSONT.Parser Core.Address
addressParser = withObject "Address" $ \v -> Core.Address
        <$> optional (geoCoordinatesParser v)
        <*> v .:? "lokalizacja_ulica"
        <*> v .:? "lokalizacja_dzielnica"
        <*> v .:? "lokalizacja_miejscowosc"

trim :: String -> String
trim = unwords . words

removeSpaces :: String -> String
removeSpaces []       = []
removeSpaces (' ':xs) = removeSpaces xs
removeSpaces (x:xs)   = x : removeSpaces xs

eitherToScraper :: Either String a -> Scraper String a
eitherToScraper = either fail return

scrapeAddress :: Scraper String Core.Address
scrapeAddress = do
    scripts <- unwords <$> texts "script"
    let matches = scripts =~ ("const addressObject\\s*=\\s*(.*);" :: String) :: [[String]]
    match <- trim <$> eitherToScraper (headErr "Couldn't find address object!" matches >>= (\x -> atErr "Couldn't match address object!" x 1))

    let
        jsonObj :: Either String JSONT.Value
        jsonObj = maybe (Left "Address is not a valid JSON object") Right $ JSON.decode $ BLU.fromString match
    eitherToScraper $ jsonObj >>= JSONT.parseEither addressParser

paramScraper :: Scraper String (String,String)
paramScraper = do
    name <- text "span"
    value <- unwords . fmap trim <$> texts ("b" @: [hasClass "parameters__value"])

    return (name, value)

type ParamKey = String
type ParamValue = String
type RawEstateParams = Map String String

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

extractParam :: ParamKey -> (ParamValue -> Maybe a) -> State RawEstateParams (Maybe a)
extractParam key f = runMaybeT $ do
    params <- get
    param <- liftMaybe $ lookup key params

    result <- liftMaybe $ f param
    put $ delete key params

    return result

extractNumbers :: String -> Maybe Int
extractNumbers x = read . head <$> headMay ((x =~ ("[0-9]+" :: String)) :: [[String]])

getEstateParameters :: RawEstateParams -> Core.EstateParameters
getEstateParameters = evalState $ do
    area <- extractParam "Powierzchnia w m2" extractNumbers
    rooms <- extractParam "Liczba pokoi" extractNumbers
    floorNo <- extractParam "Piętro" extractNumbers
    yearBuild <- extractParam "Rok budowy" extractNumbers
    buildingType <- extractParam "Typ zabudowy" Just
    totalFloors <- extractParam "Liczba pięter w budynku" extractNumbers
    refNumber <- extractParam "Numer referencyjny" Just
    parkingSpots <- extractParam "Liczba miejsc parkingowych" extractNumbers

    gets (Core.EstateParameters area rooms floorNo yearBuild buildingType totalFloors refNumber parkingSpots)

paramsScraper :: Scraper String Core.EstateParameters
paramsScraper = getEstateParameters . fromList <$> chroots ("ul" @: [hasClass "parameters__singleParameters"] // "li") paramScraper

scrapeOffer :: URL -> IO (Maybe Core.Offer)
scrapeOffer url = do
    now <- getCurrentTime

    let
        address :: Scraper String Core.Address
        address = scrapeAddress
        title :: Scraper String String
        title = trim <$> text ("h1" @: [hasClass "sticker__title"])
        estateParameters = paramsScraper
        priceS :: Scraper String (Maybe Int)
        priceS = extractNumbers . unwords . fmap removeSpaces <$> texts ("span" @: [hasClass "priceInfo__value"])
        description :: Scraper String String
        description = unwords . fmap trim <$> texts ("div" @: [hasClass "description__rolled"])
        scraper = Core.Offer url now Core.Gratka Core.Rent <$> address <*> estateParameters <*> title <*> priceS <*> description

    scrapeURL url scraper
