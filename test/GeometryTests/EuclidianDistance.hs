{-# OPTIONS_GHC -F -pgmF htfpp #-}
module GeometryTests.EuclidianDistance where
import Geometry.Space
import Geometry.Structure


import Test.Framework
import VectorTests.VectorGenerators ()


import Geometry.Math.Calculus


prop_parallelLines :: Vector3 Double -> Vector3 Double -> Bool -> Property
prop_parallelLines d v r = selector d v u (vector 100) (const True) LineT LineT
    where u = (if r then 3 else (-2)) ..* v

prop_ArbitraryLines :: Vector3 Double -> Vector3 Double -> Vector3 Double -> Property
prop_ArbitraryLines d v u = selector d v u (vector 100) (const True) LineT LineT

prop_parallelRays :: Vector3 Double -> Vector3 Double -> Bool -> Property
prop_parallelRays d v r = selector d v u (fmap abs <$> vector 100)
                                         (\(x,y) -> x >= 0 && y >= 0)
                                         RayT RayT
    where u = (if r then 3 else (-2)) ..* v

prop_ArbitraryRays :: Vector3 Double -> Vector3 Double -> Vector3 Double -> Property
prop_ArbitraryRays d v u = selector d v u (fmap abs <$> vector 100)
                                          (\(x,y) -> x >= 0 && y >= 0)
                                          RayT RayT

prop_parallelSegments :: Vector3 Double -> Vector3 Double -> Bool -> Property
prop_parallelSegments d v r = selector d v u (vectorOf 100 $ choose (0,1))
                                             (\(x,y) -> x >= 0 && y >= 0 && x <= 1 && y <= 1)
                                             SegmentT SegmentT
    where u = (if r then 3 else (-2)) ..* v

prop_ArbitrarySegments :: Vector3 Double -> Vector3 Double -> Vector3 Double -> Property
prop_ArbitrarySegments d v u = selector d v u (vectorOf 100 $ choose (0,1))
                                              (\(x,y) -> x >= 0 && y >= 0 && x <= 1 && y <= 1)
                                              SegmentT SegmentT


selector :: (Testable (m Property), Monad m)
         => Vector3 Double
         -> Vector3 Double
         -> Vector3 Double
         -> m [Double]
         -> ((Double, Double) -> Bool)
         -> LineSetType
         -> LineSetType
         -> Property
selector d v u param checker t1 t2 = property . flip runApprox 0.000001 $ do
        invalid <- return (||) <*> isSmall v <*> isSmall u
        (x,y) <- closestPoints' d v u t1 t2
        eps <- getEps
        let dd = normL2 (d .+ y ..* u .- x ..* v)
            dev x0 y0 = normL2 (d .+ y0 ..* u .- x0 ..* v) - dd
            failed = do
                xs <- param
                ys <- param
                let xs' = 0:0:1:1:xs
                    ys' = 0:1:0:1:ys
                    ds = zipWith dev xs' ys'
                return . filter (\(a,_,_) -> a < -eps) $ zip3 ds xs' ys'
        return $ do
            f <- failed
            return $ whenFail (reportProblem d v u (x,y) f) (not invalid ==> null f && checker (x,y))


reportProblem :: Vector3 Double -> Vector3 Double -> Vector3 Double
              -> (Double,Double) -> [(Double,Double,Double)] -> IO ()
reportProblem d v u (x,y) rxy0s = do
    putStrLn $ "d = " ++ show d
    putStrLn $ "v = " ++ show v
    putStrLn $ "u = " ++ show u
    putStrLn $ "u.v = " ++ show (u .*. v)
    putStrLn $ "u.d = " ++ show (u .*. d)
    putStrLn $ "v.d = " ++ show (v .*. d)
    putStrLn $ "Coords = " ++ show (x ..* v, d .+ y ..* u)
    putStrLn "Better points:"
    mapM_ print rxy0s
    putStrLn $ "Parametric (x,y) = " ++ show (x,y)
    putStrLn $ "(Ddx, Ddy) = " ++ show (dfdx x y, dfdy x y)
    putStrLn $ "Evaluated dist = " ++ show dd
    where dd = normL2 (d .+ y ..* u .- x ..* v)
          dfdx x0 y0 =  derivative' FivePoint 0.0001 (\x1 -> normL2 (d .+ y0 ..* u .- x1 ..* v)) x0
          dfdy x0 =  derivative' FivePoint 0.0001 (\y1 -> normL2 (d .+ y1 ..* u .- x0 ..* v))


