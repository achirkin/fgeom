module Main where

--import Criterion.Main

import Geometry.Math.Statistics
import Geometry.Space.Types

-- Our benchmark harness.
--main :: IO ()
--main = defaultMain [
--  bgroup "scalar mean"
--               [ bench "100"  $ nf (mean' . flip take [(1::Double)..]) 100
--               , bench "1000"  $ nf (mean' . flip take [(1::Double)..]) 1000
--               , bench "10^5"  $ nf (mean' . flip take [(1::Double)..]) 100000
--               , bench "10^7" $ nf (mean' . flip take [(1::Double)..]) 10000000
--               ]
--  ]

-- To test type $ cabal bench && hp2ps -e8in -c VectorBenches.hp

main :: IO ()
main = print $ mean . take 100000000 $ zipWith Vector2 [(3::Double)..] [(1::Double)..]
