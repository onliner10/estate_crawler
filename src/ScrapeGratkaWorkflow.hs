module ScrapeGratkaWorkflow (scrapeGratkaWorkflow) where

import           Control.Monad                  (MonadPlus (mzero), void)
import           Control.Monad.Logger           (LoggingT, runStdoutLoggingT)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Maybe      (MaybeT, runMaybeT)
import           Control.Monad.Trans.Reader     (runReaderT, ReaderT, ask)
import           Database.Persist.Postgresql    (PersistStoreWrite (insert),
                                                 SqlPersistT,
                                                 withPostgresqlConn, ConnectionString)
import           GratkaScraper                  (ListingPage (..),
                                                 scrapeListingPage, scrapeOffer)
import           PsqlPersistence                (offerToDto)

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connString action = runStdoutLoggingT $ withPostgresqlConn connString $ \backend -> runReaderT action backend

hoistMaybe :: MonadPlus m => Maybe a -> MaybeT m a
hoistMaybe = maybe mzero return

offerScraper :: String -> ReaderT ConnectionString IO ()
offerScraper url = void $ runMaybeT $ do
    offerM <- lift $ lift $ scrapeOffer url
    offer <- hoistMaybe offerM

    connString <- lift ask

    void $ lift $ lift $ runAction connString $ insert $ offerToDto offer

pageScraper :: String -> ReaderT ConnectionString IO ()
pageScraper url = void $ runMaybeT $ do
    resultsM <- lift $ lift $ scrapeListingPage url
    results <- hoistMaybe resultsM

    lift $ mapM_ offerScraper (getOfferLinks results)

    nextPage <- hoistMaybe $ getNextPage results

    lift $ pageScraper nextPage

-- TODO: Add logging
scrapeGratkaWorkflow :: ReaderT ConnectionString IO ()
scrapeGratkaWorkflow = 
    pageScraper "https://gratka.pl/nieruchomosci/mieszkania/warszawa/wynajem?powierzchnia-w-m2:min=40&liczba-pokoi:min=2&cena-calkowita:max=3000"
