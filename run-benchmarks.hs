{-# LANGUAGE NamedFieldPuns #-}

-- | HSBencher script to run all the benchmarks.
module Main where
import HSBencher
import HSBencher.Backend.Fusion  (defaultFusionPlugin)
import HSBencher.Backend.Dribble (defaultDribblePlugin)

import Data.Monoid  (mappend)
-- import qualified Data.Map as M
-- import System.Environment (getEnvironment)
-- import System.Directory   (setCurrentDirectory, getDirectoryContents, getCurrentDirectory)
-- import System.IO.Unsafe   (unsafePerformIO)
-- import System.Process     
-- import GHC.Conc           (getNumProcessors)

--------------------------------------------------------------------------------


benches :: [Benchmark DefaultParamMeaning]
benches = 
  [ (mkBenchmark ("cilk_tests/parfib/Makefile") [] params) { progname = Just "parfib" }
  ]
 where 
   params = Or [ Set NoMeaning (RuntimeParam (show sz))
               | sz <- [1 .. 10 :: Int] ]


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

