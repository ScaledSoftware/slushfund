module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time

type CountryCode = Int


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

fundOtherPerson :: PersonId -> Fund -> PersonId
fundOtherPerson pId fund = if fundP1 fund == pId
                       then fundP2 fund
                       else fundP1 fund

myDebtFromFundEntry :: PersonId -> FundEntry -> Double
myDebtFromFundEntry myPId fundEntry =
    if myPId == fundEntryPayer fundEntry
      then -1.0 * fairShare
      else fairShare
  where
    fairShare = if fundEntryWasSplit fundEntry 
                  then 0.5 * fundEntryAmount fundEntry
                  else fundEntryAmount fundEntry

