{-# LANGUAGE NamedFieldPuns #-}

-- | HSBencher script to run all the benchmarks.
module Main where
import HSBencher
import HSBencher.Backend.Fusion  (defaultFusionPlugin)
import HSBencher.Backend.Dribble (defaultDribblePlugin)
import HSBencher.Backend.Codespeed (defaultCodespeedPlugin, CodespeedConfig(..))

import Data.Default (Default(def))
import Data.Monoid  (mappend)
import GHC.Conc (getNumProcessors)
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------


-- We'll uncomment these later
benches :: [Benchmark DefaultParamMeaning]
benches =
   [ (mkBenchmark "cilk_tests/parfib/Makefile"                        [ ] parfibParams     ) { progname = Just "parfib"}]            ++ 
--   [ (mkBenchmark "cilk_tests/ivar/benchmarks/microbench/Makefile"    [ ] microbenchParams ) { progname = Just "microbench"}]        ++ 

   [ (mkBenchmark "cilk_tests/ivar/benchmarks/microbench/Makefile"    [ ] microbench_allblockParams ) 
                                                                      { progname = Just "microbench_allblock"}]  ++ 
   [ (mkBenchmark "cilk_tests/ivar/benchmarks/microbench/Makefile"    [ ] microbench_raceParams ) 
                                                                      { progname = Just "microbench_race"}]  ++ 
   [ (mkBenchmark "cilk_tests/ivar/benchmarks/microbench/Makefile"    [ ] microbench_noblockParams ) 
                                                                      { progname = Just "microbench_noblock"}]  ++ 


   --[ (mkBenchmark "cilk_tests/ivar/benchmarks/wavefront/Makefile"     [ ] wavefrontParams  ) { progname = Just "wavefront"}]         ++
   [ (mkBenchmark "cilk_tests/ivar/benchmarks/pingpong/Makefile"      [ ] pingpongParams  )  { progname = Just "pingpong"}]          ++ 
 
   [ (mkBenchmark "cilk_tests/regression/black-scholes/Makefile"      [ ] scholesParams    ) { progname = Just "black-scholes"}]     ++ 
   --[ (mkBenchmark "cilk_tests/regression/cholesky/Makefile"          [ ] choleskyParams   ) { progname = Just "cholesky"}]          ++ 
   --[ (mkBenchmark "cilk_tests/regression/jacobi-heat/Makefile"       [ ] jacobiParams     ) { progname = Just "jacobi-heat"}]       ++ 
   [ (mkBenchmark "cilk_tests/regression/kalah/Makefile"             [ ] kalahParams      ) { progname = Just "kalah"}]             ++ 
   [ (mkBenchmark "cilk_tests/regression/knapsack/Makefile"          [ ] knapsackParams   ) { progname = Just "knapsack"}]          ++ 
   [ (mkBenchmark "cilk_tests/regression/LU_decomp/Makefile"         [ ] luParams         ) { progname = Just "LU_decomp"}]         ++ 
   [ (mkBenchmark "cilk_tests/regression/magic-numbers/Makefile"     [ ] magicNumsParams  ) { progname = Just "magic-numbers"}]     ++ 
   [ (mkBenchmark "cilk_tests/regression/strassen_multiply/Makefile" [ "-n", "4096" ] strassenParams   ) { progname = Just "strassen-multiply"}]

-- Set this so that HSBencher actually runs the tests that we are not passing any
-- parameter to (at least currently)

emptyParams, microbench_allblockParams, microbench_raceParams, microbench_noblockParams,
  pingpongParams, wavefrontParams, 
  scholesParams, choleskyParams, jacobiParams, kalahParams, knapsackParams,
   luParams, magicNumsParams, strassenParams, parfibParams :: BenchSpace DefaultParamMeaning

emptyParams      = varyCilkThreads $ Or [Set NoMeaning (CompileParam $ show (10::Int))]

microbench_noblockParams  = varyThreads [2] $ mb mb_szs mb_itrs ["microbench_noblock"]
microbench_raceParams     = varyThreads [2] $ mb mb_szs mb_itrs ["microbench_race"]
-- Note, the allblock version spins the writer... can't run on 1 thread:
microbench_allblockParams = varyThreads [2] $ mb mb_szs mb_itrs ["microbench_allblock"]

mb_szs :: [Int]
mb_szs  = (map (10^) [ 0 .. 4::Int ])

mb_itrs :: [Int]
mb_itrs = (map (10^) [ 0 .. 3::Int ])

mb :: [Int] -> [Int] -> [String] -> BenchSpace DefaultParamMeaning
mb fbrs iters vars = 
  Or [ And [ Set NoMeaning (RuntimeArg $ unwords [show fbr, show iter])
           , Set (Variant var) (RuntimeEnv "VARIANT" var) ]
     | var <- vars, fbr <- fbrs, iter <- iters ]
----------------------------------------

pingpongParams  = varyCilkThreads $ 
                   And [ Or [ Set NoMeaning (RuntimeArg $ unwords [show pairs, show iters]) 
                            | pairs <- [ 1, 2, 4, 8 ]     :: [Int]
                            , iters <- [ 100, 500, 1000 ] :: [Int] ]
                       , Set (NoMeaning) (RuntimeEnv "VARIANT" "pingpong_ivars") ] 

wavefrontParams  = varyCilkThreads emptyParams
scholesParams    = varyCilkThreads emptyParams
choleskyParams   = varyCilkThreads emptyParams
jacobiParams     = varyCilkThreads emptyParams
kalahParams      = varyCilkThreads emptyParams
knapsackParams   = varyCilkThreads emptyParams
luParams         = varyCilkThreads emptyParams
magicNumsParams  = varyCilkThreads emptyParams
strassenParams   = varyCilkThreads emptyParams
parfibParams     = varyCilkThreads $ 
                    Or [ pfibs [10, 15, 20, 25, 30, 35, 40, 41, 42] ["parfib", "ivars_parfib" ]
                       -- These are running only on MUCH smaller sizes:
                       , pfibs [10, 11, 12, 13] ["fib_pthread"] ]
 where 
   pfibs :: [Int] -> [String] -> BenchSpace DefaultParamMeaning
   pfibs szs vars = 
     And [ Or [ Set NoMeaning (RuntimeArg $ show sz) | sz <- szs ]
         , Or [ Set (Variant var) (RuntimeEnv "PARFIB_VARIANT" var) | var <- vars ] ]


-- | GHC specific method of varying threads.
varyCilkThreads :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
varyCilkThreads = varyThreads threadSelection

-- | Skip the one-thread version.
varyCilkThreadsParOnly :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
varyCilkThreadsParOnly = varyThreads (filter (>1) threadSelection)

varyThreads :: [Int] -> BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
varyThreads threads conf = And [ conf, Or (map fn threads) ]
 where
   fn n = Set (Threads n) $ RuntimeEnv "CILK_NWORKERS" (show n)

-- | Default threading settings based on the number of processors on the current machine.
threadSelection :: [Int]
threadSelection = unsafePerformIO $ do
  p   <- getNumProcessors
  return$
    if p <= 4  then [1..p] else
    if p <= 16 then 1: [2,4 .. p]
    else            1:2:[4,8 .. p]

-- Now dow we make these parameters change based upon where we are at in the dir tree?
--trivialParams = Or [Set NoMeaning (RuntimeParam (show 10))]

main :: IO ()
main = do
  putStrLn "Begin Concurrent Cilk profiling benchmarks..."
  defaultMainModifyConfig $ \ conf ->
    addPlugin defaultDribblePlugin def $ 
    addPlugin defaultFusionPlugin def  $ 
    addPlugin defaultCodespeedPlugin csconf $ 
    conf{ benchlist  = benches
                       -- 1 hour timeout
        , runTimeOut = Just 120 -- Erk... need a separate compile timeout.
        , harvesters = customTagHarvesterInt "CILKPLUS_SYSTEM_WORKERS" `mappend` 
                       customTagHarvesterInt "CILKPLUS_USER_WORKERS" `mappend` 
                       customTagHarvesterInt "CILKPLUS_RUNTIME_MEMORY_USAGE_BYTES" `mappend` 
                       customTagHarvesterInt "CILKPLUS_STACKSIZE" `mappend` 
                       customTagHarvesterInt "CILKPLUS_TOTALSTACKS" `mappend` 
                       customTagHarvesterInt "CONCURRENTCILK_WORKERS_BLOCKED" `mappend` 
                       harvesters conf
        }
 where
  -- Some default settings for benchmarking:
  csconf = def { codespeedURL = "http://codespeed.crest.iu.edu"
               , projName     = "ConcurrentCilk" }
