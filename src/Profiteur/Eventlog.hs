--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings          #-}
module Profiteur.Eventlog
    ( decodeEventlog
    ) where

--------------------------------------------------------------------------------

import qualified GHC.RTS.Events              as Ev
import qualified GHC.Events2Prof             as Ev2Prof

--------------------------------------------------------------------------------
import Profiteur.Core
import Profiteur.Parser

--------------------------------------------------------------------------------
decodeEventlog :: Ev.EventLog -> Either String CostCentre
decodeEventlog = profileToCostCentre . Ev2Prof.eventlogToProfile
