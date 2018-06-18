{-# language OverloadedStrings #-}

-- | This module defines a metrics that exposes statistics from the GHC runtime
-- system ("GHC.Conc", "GHC.Stats").
--
-- To use these metrics, the monitored executable should run with the `+RTS -T`
-- command line flags and the following must be added somewhere near the
-- beginning of the main method:
--
-- >>> register ghcMetrics
module Prometheus.Metric.GHC (
    GHCMetrics
,   ghcMetrics
) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.UTF8 as BS
import Data.Text (Text)
import GHC.Conc (numSparks, getNumCapabilities)
import GHC.Stats
import Prometheus


data GHCMetrics = GHCMetrics

ghcMetrics :: Metric GHCMetrics
ghcMetrics = Metric (return (GHCMetrics, concat <$> sequence ghcCollectors))

ghcCollectors :: [IO [SampleGroup]]
ghcCollectors = [
        showCollector
            "ghc_sparks"
            "The number of sparks in the local spark pool."
            GaugeType
            numSparks
    ,   showCollector
            "ghc_capabilities"
            "The number of threads that can run truly simultaneously."
            GaugeType
            getNumCapabilities
    ,   statsCollector
            "ghc_allocated_bytes_total"
            "Total number of bytes allocated."
            CounterType
            allocated_bytes
    ,   statsCollector
            "ghc_num_gcs"
            "The number of garbage collections performed."
            CounterType
            gcs
    ,   statsCollector
            "ghc_max_used_bytes"
            "The maximum number of live bytes seen so far."
            GaugeType
            max_live_bytes
    ,   statsCollector
            "ghc_cumulative_used_bytes_total"
            "The cumulative total bytes used."
            CounterType
            cumulative_live_bytes
    ,   statsCollector
            "ghc_copied_bytes_total"
            "The number of bytes copied during garbage collection."
            CounterType
            copied_bytes
    ,   statsCollector
            "ghc_current_used_bytes"
            "The number of current live bytes."
            GaugeType
            (gcdetails_live_bytes . gc)
    ,   statsCollector
            "ghc_current_slop_bytes"
            "The current number of bytes lost to slop."
            GaugeType
            (gcdetails_slop_bytes . gc)
    ,   statsCollector
            "ghc_max_slop_bytes"
            "The maximum number of bytes lost to slop so far."
            GaugeType
            max_slop_bytes
    ,   statsCollector
            "ghc_peak_allocated_megabytes" -- XXX: export as bytes?
            "The maximum number of megabytes allocated."
            GaugeType
            max_mem_in_use_bytes
    ,   statsCollector
            "ghc_mutator_cpu_seconds_total"
            "The CPU time spent running mutator threads."
            CounterType
            mutator_cpu_ns
    ,   statsCollector
            "ghc_mutator_wall_seconds_total"
            "The wall clock time spent running mutator threads."
            CounterType
            mutator_elapsed_ns
    ,   statsCollector
            "ghc_gc_cpu_seconds_total"
            "The CPU time spent running GC."
            CounterType
            gc_cpu_ns
    ,   statsCollector
            "ghc_gc_wall_seconds_total"
            "The wall clock time spent running GC."
            CounterType
            gc_elapsed_ns
    ,   statsCollector
            "ghc_cpu_seconds_total"
            "Total CPU time elapsed since program start."
            CounterType
            cpu_ns
    ,   statsCollector
            "ghc_wall_seconds_total"
            "Total wall clock time elapsed since start."
            CounterType
            elapsed_ns
    ,   statsCollector
            "ghc_parallel_copied_bytes_total"
            "Number of bytes copied during GC, minus space held by mutable lists held by the capabilities."
            CounterType
            par_copied_bytes
    ,   statsCollector
            "ghc_parallel_max_copied_bytes_total"
            "Sum of number of bytes copied each GC by the most active GC thread each GC."
            CounterType
            cumulative_par_max_copied_bytes
    ]

statsCollector :: Show a
               => Text -> Text -> SampleType -> (RTSStats -> a) -> IO [SampleGroup]
statsCollector name help sampleType stat = do
    statsEnabled <- getRTSStatsEnabled
    if statsEnabled
        then showCollector name help sampleType (stat <$> getRTSStats)
        else return []

showCollector :: Show a => Text -> Text -> SampleType -> IO a -> IO [SampleGroup]
showCollector name help sampleType ioInt = do
    value <- ioInt
    let info = Info name help
    let valueBS = BS.fromString $ show value
    return [SampleGroup info sampleType [Sample name [] valueBS]]
