{-# LANGUAGE NamedFieldPuns #-}

-- | HSBencher script to run all the benchmarks.
module Main where
import HSBencher
import HSBencher.Backend.Fusion  (defaultFusionPlugin)
import HSBencher.Backend.Dribble (defaultDribblePlugin)
import HSBencher.Backend.Fusion  (defaultFusionPlugin) 

import Data.Monoid  (mappend)
import qualified Data.Map as M
import System.Environment (getEnvironment)
import System.Directory   (setCurrentDirectory, getDirectoryContents, getCurrentDirectory)
import System.IO.Unsafe   (unsafePerformIO)
import System.Process     
import GHC.Conc           (getNumProcessors)

--------------------------------------------------------------------------------

fixed_backoffLevels :: [Integer]
-- fixed_backoffLevels = [ 10^i | i <- [0..6] ]
fixed_backoffLevels = [ 1, 10, 100 ]
-- fixed_backoffLevels = [1,5,10,50,100,500,1000,5000,10000,50000,100000,500000,1000000,5000000]

-- fixed_backoffLevels = [ round (10 ** i) | i <- [0, 0.5 .. 6.5] ]

-- [2014.08.07] Going bigger:
-- fixed_backoffLevels = [ 10^7, 10^8 ]

benches :: [Benchmark DefaultParamMeaning]
benches = 
  [ (mkBenchmark ("cilk_tests/parfib/Makefile") [show sz] trivialParams) 
     { progname = Just "parfib" }
  | sz <- [1 .. 10]
  ]

trivialParams = And []

setVariant str = (And [Set (Variant str) (CompileParam "")])

main :: IO ()
main = do
  putStrLn "Begin Concurrent Cilk profiling benchmarks..."
  defaultMainModifyConfig $ \ conf ->
    conf{ benchlist  = benches
                       -- 1 hour timeout
        , runTimeOut = Just 1000 -- Erk... need a separate compile timeout.
        , plugIns   = [ SomePlugin defaultFusionPlugin,
                        SomePlugin defaultDribblePlugin ]
        , harvesters = customTagHarvesterDouble "CILK_BLAH_BLAH" `mappend` 
                       harvesters conf
        }

