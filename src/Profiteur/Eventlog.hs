--------------------------------------------------------------------------------
module Profiteur.Eventlog
    ( decodeEventlog
    ) where

--------------------------------------------------------------------------------

import qualified GHC.Prof        as Prof
import qualified GHC.Prof.Types  as Prof
import qualified GHC.RTS.Events  as Ev

--------------------------------------------------------------------------------
import Profiteur.Core
import Profiteur.Parser

--------------------------------------------------------------------------------
decodeEventlog :: Ev.EventLog -> Either String CostCentre
decodeEventlog evlog = evlogToProfile evlog >>= profileToCostCentre

--------------------------------------------------------------------------------

evlogToProfile :: Ev.EventLog -> Either String Prof.Profile
evlogToProfile ev = Left "not implemented"
