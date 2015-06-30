module Main where

import Criterion.Main

import Geometry.Math.Statistics
--import Geometry.Space.Types

-- Our benchmark harness.
main :: IO ()
main = defaultMain [
  bgroup "scalar mean"
               [ bench "100"  $ whnf (mean' . flip take [(1::Double)..]) 100
               , bench "1000"  $ whnf (mean' . flip take [(1::Double)..]) 1000
               , bench "10^5"  $ whnf (mean' . flip take [(1::Double)..]) 100000
               , bench "10^6" $ whnf (mean' . flip take [(1::Double)..]) 1000000
               ]
  ]

-- To test type $ cabal bench && hp2ps -e8in -c VectorBenches.hp

--main :: IO ()
--main = print $ mean' . take 100000000 $ [(1::Double)..]
--main = print $ mean . take 100000000 $ zipWith Vector2 [(3::Double)..] [(1::Double)..]

--main :: IO ()
--main = defaultMain [
--    bgroup "scalar mean"
--               [ bench "100"   $ nfIO (print $ mean' . take 100 $ [(1::Double)..])
--               , bench "1000"  $ nfIO (print $ mean' . take 1000 $ [(1::Double)..])
--               , bench "10^5"  $ nfIO (print $ mean' . take 100000 $ [(1::Double)..])
--               , bench "10^7"  $ nfIO (print $ mean' . take 10000000 $ [(1::Double)..])
--               ]
--    ]
