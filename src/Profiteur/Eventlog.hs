--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings          #-}
module Profiteur.Eventlog
    ( decodeEventlog
    ) where

--------------------------------------------------------------------------------

import Data.Word (Word32, Word64)

import qualified Data.IntMap.Strict as IM
import qualified Data.List          as L
import qualified Data.Map.Strict    as M
import qualified Data.Set           as S
import qualified Data.Text          as T
import qualified GHC.Prof           as Prof
import qualified GHC.Prof.Types     as Prof
import qualified Profiteur.Eventlog.Prof     as Prof
import qualified GHC.RTS.Events              as Ev
import qualified Data.Vector.Unboxed         as VU

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
    Ev.ProgramArgs _ args ->
        acc { accCommandLine = Just args }
    Ev.ProfBegin ti ->
        acc { accTickInterval = Just ti }
    Ev.HeapProfCostCentre ccid label m loc _flags ->
        acc { accCostCentres = M.insert ccid (CC label m loc) (accCostCentres acc) }
    Ev.ProfSampleCostCentre _capset ticks _sd stack ->
        acc { accSamples = addSample stack ticks (accSamples acc) }
    _ -> acc

accToProfile :: Acc -> Prof.Profile
accToProfile acc = Prof.Profile
    { Prof.profileTimestamp      = Time.LocalTime (Time.ModifiedJulianDay 0) (Time.TimeOfDay 0 0 0)
    , Prof.profileCommandLine    = maybe "" T.unwords (accCommandLine acc)
    , Prof.profileTotalTime      = Prof.TotalTime
        { Prof.totalTimeElapsed    = 0
        , Prof.totalTimeTicks      = 0
        , Prof.totalTimeResolution = 0
        , Prof.totalTimeProcessors = Nothing
        }
    , Prof.profileTotalAlloc     = Prof.TotalAlloc 0
    , Prof.profileTopCostCentres = [] -- we don't calculate these atm.
    , Prof.profileCostCentreTree = Prof.buildTree [] -- TODO
    }

addSample
    :: VU.Vector Word32
    -> Word64
    -> M.Map (VU.Vector CostCentreId) Sample
    -> M.Map (VU.Vector CostCentreId) Sample
addSample stack ticks m = M.insertWith sampleAppend stack (Sample ticks 1) m


type CostCentreId = Word32

data Acc = Acc
    { accCommandLine  :: !(Maybe [T.Text])   -- ^ The command-line arguments passed to the program. @PROGRAM_ARGS@
    , accTickInterval :: !(Maybe Word64)     -- ^ tick interval in nanoseconds. @PROF_BEGIN@
    , accCostCentres  :: !(M.Map CostCentreId CC)
    , accSamples      :: !(M.Map (VU.Vector CostCentreId) Sample)
    }
  deriving Show

emptyAcc :: Acc
emptyAcc = Acc
    { accCommandLine  = Nothing
    , accTickInterval = Nothing
    , accCostCentres  = M.empty
    , accSamples      = M.empty
    }

data CC = CC
    { ccLabel  :: !T.Text
    , ccModule :: !T.Text
    , ccSrcLoc :: !T.Text
    }
  deriving Show

data Sample = Sample
    { sTicks   :: !Word64
    , sEntries :: !Word64
    }
  deriving Show

sampleAppend :: Sample -> Sample -> Sample
sampleAppend (Sample x1 x2) (Sample y1 y2) = Sample (x1 + y1) (x2 + y2)

--------------------------------------------------------------------------------
