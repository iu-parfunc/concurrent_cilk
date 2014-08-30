{-# LANGUAGE NamedFieldPuns #-}

-- | HSBencher script to run all the benchmarks.
module Main where
import HSBencher
import HSBencher.Backend.Fusion  (defaultFusionPlugin)
import HSBencher.Backend.Dribble (defaultDribblePlugin)

import Data.Monoid  (mappend)

--------------------------------------------------------------------------------


-- We'll uncomment these later
benches :: [Benchmark DefaultParamMeaning]
benches =
   [ (mkBenchmark "cilk_tests/parfib/Makefile"                        [ ] parfibParams     ) { progname = Just "parfib"}]            ++ 
   [ (mkBenchmark "cilk_tests/ivar/benchmarks/microbench/Makefile"    [ ] microbenchParams ) { progname = Just "microbench"}]        ++ 
   --[ (mkBenchmark "cilk_tests/ivar/benchmarks/wavefront/Makefile"     [ ] wavefrontParams  ) { progname = Just "wavefront"}]         ++ 
   [ (mkBenchmark "cilk_tests/regression/black-scholes/Makefile"      [ ] scholesParams    ) { progname = Just "black-scholes"}]     ++ 
   --[ (mkBenchmark "cilk_tests/regression/cholesky/Makefile"          [ ] choleskyParams   ) { progname = Just "cholesky"}]          ++ 
   --[ (mkBenchmark "cilk_tests/regression/jacobi-heat/Makefile"       [ ] jacobiParams     ) { progname = Just "jacobi-heat"}]       ++ 
   [ (mkBenchmark "cilk_tests/regression/kalah/Makefile"             [ ] kalahParams      ) { progname = Just "kalah"}]             ++ 
   [ (mkBenchmark "cilk_tests/regression/knapsack/Makefile"          [ ] knapsackParams   ) { progname = Just "knapsack"}]          ++ 
   [ (mkBenchmark "cilk_tests/regression/LU_decomp/Makefile"         [ ] luParams         ) { progname = Just "LU_decomp"}]         ++ 
   [ (mkBenchmark "cilk_tests/regression/magic-numbers/Makefile"     [ ] magicNumsParams  ) { progname = Just "magic-numbers"}]     ++ 
   [ (mkBenchmark "cilk_tests/regression/strassen_multiply/Makefile" [ ] strassenParams   ) { progname = Just "strassen-multiply"}]

-- Set this so that HSBencher actually runs the tests that we are not passing any
-- parameter to (at least currently)
emptyParams      = Or [Set NoMeaning (CompileParam $ show 10)]
microbenchParams = emptyParams
wavefrontParams  = emptyParams
scholesParams    = emptyParams
choleskyParams   = emptyParams
jacobiParams     = emptyParams
kalahParams      = emptyParams
knapsackParams   = emptyParams
luParams         = emptyParams
magicNumsParams  = emptyParams
strassenParams   = emptyParams
parfibParams     = Or [Set NoMeaning (RuntimeParam $ show sz) | sz <- [10, 15..35]]

-- Now dow we make these parameters change based upon where we are at in the dir tree?
--trivialParams = Or [Set NoMeaning (RuntimeParam (show 10))]

main :: IO ()
main = do
  putStrLn "Begin Concurrent Cilk profiling benchmarks..."
  defaultMainModifyConfig $ \ conf ->
    conf{ benchlist  = benches
                       -- 1 hour timeout
        , runTimeOut = Just 100 -- Erk... need a separate compile timeout.
        , plugIns   = [ SomePlugin defaultFusionPlugin,
                        SomePlugin defaultDribblePlugin ]
        , harvesters = customTagHarvesterDouble "CILK_NWORKERS" `mappend` 
                       harvesters conf
        }

