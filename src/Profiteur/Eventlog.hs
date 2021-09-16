--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings          #-}
module Profiteur.Eventlog
    ( decodeEventlog
    ) where

--------------------------------------------------------------------------------

import Data.Word (Word32, Word64)

import qualified Data.List                   as L
import qualified Data.Map.Strict             as M
import qualified Data.Scientific             as S
import qualified Data.Text                   as T
import qualified Data.Time                   as Time
import qualified Data.Vector.Unboxed         as VU
import qualified GHC.Prof                    as Prof
import qualified GHC.RTS.Events              as Ev
import qualified Profiteur.Eventlog.Prof     as Prof

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
        acc { accCostCentres = M.insert ccid (CC label m loc) (accCostCentres acc) } -- TODO: read _flags whether the ccid is a CAF
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
    , Prof.profileCostCentreTree = Prof.buildTree $ costCentres (accSamples acc) (accCostCentres acc)
    }

addSample
    :: VU.Vector Word32
    -> Word64
    -> M.Map (VU.Vector CostCentreId) Sample
    -> M.Map (VU.Vector CostCentreId) Sample
addSample stack ticks m = M.insertWith sampleAppend stack (Sample ticks 1) m

-- | Convert accumulated data into list of levels and costcentres,
-- so ghc-prof can build its CostCentreTree.
costCentres
    :: M.Map (VU.Vector CostCentreId) Sample
    -> (M.Map CostCentreId CC)
    -> [(Prof.Level, Prof.CostCentre)]
costCentres m0 cc0 = traceShowId
    [ (VU.length ccid, mkCostCentre n ccid as)
    | (n, (ccid, as)) <- zip [1..] (M.toList accSampleMap)
    ]
  where
    mkCostCentre :: Int -> VU.Vector CostCentreId -> AccSample -> Prof.CostCentre
    mkCostCentre no ccid as = Prof.CostCentre
        { Prof.costCentreNo       = no
        , Prof.costCentreName     = ccLabel cc
        , Prof.costCentreModule   = ccMod cc
        , Prof.costCentreSrc      = Just (ccSrcLoc cc)
        , Prof.costCentreEntries  = toInteger (sEntries s) -- no idea what this is
        , Prof.costCentreIndTime  = ticksToTime (sTicks s)
        , Prof.costCentreIndAlloc = 0 -- TODO
        , Prof.costCentreInhTime  = ticksToTime (asTicks as)
        , Prof.costCentreInhAlloc = 0 -- TODO
        , Prof.costCentreTicks    = Just (toInteger (sTicks s))
        , Prof.costCentreBytes    = Nothing -- TODO
        }
      where
        cc | VU.null ccid = mainCC
           | otherwise    = M.findWithDefault defaultCC (VU.last ccid) cc0
        s =  M.findWithDefault zeroSample (VU.reverse ccid) m0

    ticksToTime :: Word64 -> S.Scientific
    ticksToTime ticks = S.fromFloatDigits (100 * fromIntegral ticks / totalTicks)

    -- time (and allocation) are represented in percentages.
    totalTicks :: Double
    totalTicks = fromIntegral $ M.foldl' (\ !acc s -> acc + sTicks s) 0 m0

    -- For each call stack we take partial callstacks and accumulate data.
    accSampleMap :: M.Map (VU.Vector CostCentreId) AccSample
    accSampleMap = M.foldlWithKey' f M.empty m0
      where
        f :: M.Map (VU.Vector CostCentreId) AccSample
          -> VU.Vector CostCentreId
          -> Sample
          -> M.Map (VU.Vector CostCentreId) AccSample
        f m ccid s = L.foldl' (\m' ccid' -> M.insertWith accSampleAppend ccid' as m') m (partialCallstacks ccid)
          where
            as = AccSample
                { asTicks = sTicks s
                }
            
-- | 
partialCallstacks :: VU.Unbox a => VU.Vector a -> [VU.Vector a]
partialCallstacks v = [ VU.take n v' | n <- [ 0 .. VU.length v ] ] where v' = VU.reverse v

--------------------------------------------------------------------------------


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
    , ccMod    :: !T.Text
    , ccSrcLoc :: !T.Text
    }
  deriving Show

-- | Top level cost center
mainCC :: CC
mainCC = CC
    { ccLabel  = "MAIN"
    , ccMod    = "MAIN"
    , ccSrcLoc = "<built-in>"
    }

defaultCC :: CC
defaultCC = CC
    { ccLabel  = "unknown"
    , ccMod    = "unknown"
    , ccSrcLoc = "UNKNOWN"
    }

data Sample = Sample
    { sTicks   :: !Word64
    , sEntries :: !Word64
    }
  deriving Show

zeroSample :: Sample
zeroSample = Sample 0 0

sampleAppend :: Sample -> Sample -> Sample
sampleAppend (Sample x1 x2) (Sample y1 y2) = Sample (x1 + y1) (x2 + y2)

-- | Sum
data AccSample = AccSample
    { asTicks :: !Word64
    }

accSampleAppend :: AccSample -> AccSample -> AccSample
accSampleAppend (AccSample x1) (AccSample y1) = AccSample (x1 + y1)

--------------------------------------------------------------------------------
