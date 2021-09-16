--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings          #-}
module Profiteur.Eventlog
    ( decodeEventlog
    ) where

--------------------------------------------------------------------------------

import Data.Maybe (fromMaybe)
import Data.Word (Word32)

import qualified Data.IntMap.Strict as IM
import qualified Data.List          as L
import qualified Data.Map.Strict    as M
import qualified Data.Set           as S
import qualified Data.Text          as T
import qualified GHC.Prof           as Prof
import qualified GHC.Prof.Types     as Prof
import qualified GHC.RTS.Events     as Ev

import qualified Data.Time as Time

import Debug.Trace

--------------------------------------------------------------------------------
import Profiteur.Core
import Profiteur.Parser

--------------------------------------------------------------------------------
decodeEventlog :: Ev.EventLog -> Either String CostCentre
decodeEventlog evlog = evlogToProfile evlog >>= profileToCostCentre

--------------------------------------------------------------------------------
evlogToProfile :: Ev.EventLog -> Either String Prof.Profile
evlogToProfile ev = traceShow acc $ Right (accToProfile acc)
  where
    acc = L.foldl' one emptyAcc (Ev.events (Ev.dat ev))

one :: Acc -> Ev.Event -> Acc
one !acc ev = case Ev.evSpec ev of
    Ev.ProgramInvocation l ->
        acc { accCommandLine = Just (T.pack l) }
    Ev.HeapProfCostCentre ccid label m loc _flags ->
        acc { accCostCentres = M.insert ccid (CC label m loc) (accCostCentres acc) }
    Ev.ProfBegin ti -> acc -- TODO: record tick interval
    Ev.ProfSampleCostCentre _ ticks sd stack -> acc -- TODO: record sample
    _ -> acc

accToProfile :: Acc -> Prof.Profile
accToProfile acc = Prof.Profile
    { Prof.profileTimestamp      = Time.LocalTime (Time.ModifiedJulianDay 0) (Time.TimeOfDay 0 0 0)
    , Prof.profileCommandLine    = fromMaybe ""  (accCommandLine acc)
    , Prof.profileTotalTime      = Prof.TotalTime
        { Prof.totalTimeElapsed    = 0
        , Prof.totalTimeTicks      = 0
        , Prof.totalTimeResolution = 0
        , Prof.totalTimeProcessors = Nothing
        }
    , Prof.profileTotalAlloc     = Prof.TotalAlloc 0
    , Prof.profileTopCostCentres = [] -- we don't calculate these atm.
    , Prof.profileCostCentreTree = Prof.CostCentreTree
        { Prof.costCentreNodes     = IM.empty
        , Prof.costCentreParents   = IM.empty
        , Prof.costCentreChildren  = IM.empty
        , Prof.costCentreCallSites = M.empty
        , Prof.costCentreAggregate = M.empty
        }
    }

data Acc = Acc
    { accCommandLine :: !(Maybe T.Text)
    , accCostCentres :: !(M.Map Word32 CC)
    }
  deriving Show

emptyAcc :: Acc
emptyAcc = Acc
    { accCommandLine = Nothing
    , accCostCentres = M.empty
    }

data CC = CC
    { ccLabel  :: !T.Text
    , ccModule :: !T.Text
    , ccSrcLoc :: !T.Text
    }
  deriving Show

--------------------------------------------------------------------------------
