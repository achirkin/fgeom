{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MathTests.Polynomial where

import Data.Foldable (foldl')
import qualified Control.Monad as M

import Test.Framework
import Geometry.Math.Polynomial
import Geometry.Space.Approximate


prop_polyRootsR1 :: Double -> Double -> Bool
prop_polyRootsR1 a b = all (\y -> abs y <= mag*10e-15) vals
    where xs = polyRootReal poly
          poly = [b,a]
          vals = map (polyValueWide poly) xs
          mag = maximum . map (polyModulus poly) $ xs

prop_polyRootsR2 :: Double -> Double -> Double -> Bool
prop_polyRootsR2 a b c = all (\y -> abs y <= mag*10e-10) vals
    where xs = polyRootReal poly
          poly = [c,b,a]
          vals = map (polyValueWide poly) xs
          mag = maximum . map (polyModulus poly) $ xs

prop_polyRootsR3 :: Double -> Double -> Double -> Double -> Bool
prop_polyRootsR3 a b c d = all (\y -> abs y <= mag*10e-8) vals
    where xs = polyRootReal poly
          poly = [d,c,b,a]
          vals = map (polyValueWide poly) xs
          mag = maximum . map (polyModulus poly) $ xs

prop_polyRootsR4 :: Double -> Double -> Double -> Double -> Double -> Bool
prop_polyRootsR4 = polyRootsR4

prop_polyRootsI4 :: Int ->  Int ->  Int ->  Int ->  Int -> Bool
prop_polyRootsI4 a b c d e = polyRootsR4
    (fromIntegral $ mod a 1000 - 500)
    (fromIntegral $ mod b 1000 - 500)
    (fromIntegral $ mod c 1000 - 500)
    (fromIntegral $ mod d 1000 - 500)
    (fromIntegral $ mod e 1000 - 500)


prop_polyRootsR5 :: Double -> Double -> Double -> Double -> Double -> Double -> Bool
prop_polyRootsR5 = polyRootsR5

prop_polyRootsI5 :: Int ->  Int ->  Int ->  Int ->  Int ->  Int -> Bool
prop_polyRootsI5 a b c d e f = polyRootsR5
    (fromIntegral $ mod a 1000 - 500)
    (fromIntegral $ mod b 1000 - 500)
    (fromIntegral $ mod c 1000 - 500)
    (fromIntegral $ mod d 1000 - 500)
    (fromIntegral $ mod e 1000 - 500)
    (fromIntegral $ mod f 1000 - 500)


polyRootsR4 :: Double -> Double -> Double -> Double -> Double -> Bool
polyRootsR4 0 0 0 0 0 = True
polyRootsR4 a b c d e = runApprox (M.liftM and $ mapM (isSmall' . (/mag)) vals) 10e-8
    where xs = polyRootReal poly
          poly = [e,d,c,b,a]
          vals = map (polyValueWide poly) xs
          mag = maximum . map (polyModulus poly) $ xs

polyRootsR5 :: Double -> Double -> Double -> Double -> Double -> Double -> Bool
polyRootsR5 a b c d e f = all (\y -> abs y <= mag*10e-10) vals
    where xs = polyRootReal poly
          poly = [f,e,d,c,b,a]
          vals = map (polyValueWide poly) xs
          mag = maximum . map (polyModulus poly) $ xs


polyModulus :: (RealFloat a) => [a] -> a -> a
polyModulus coefs x = fst $ foldl' (\(acc,(xps,xpe)) (cs,ce) -> (max acc $ encodeFloat (cs*xps) (ce+xpe), (xps*xs,xpe+xe))) (0,decodeFloat (1::Double)) coefs'
    where (xs,xe) = decodeFloat . abs $ x
          coefs' = map (decodeFloat . abs) coefs

polyValueWide :: (RealFloat a) => [a] -> a -> a
polyValueWide coefs x = fst $ foldl' (\(acc,(xps,xpe)) (cs,ce) -> (acc + encodeFloat (cs*xps) (ce+xpe), (xps*xs,xpe+xe))) (0,decodeFloat (1::Double)) coefs'
    where (xs,xe) = decodeFloat x
          coefs' = map decodeFloat coefs
