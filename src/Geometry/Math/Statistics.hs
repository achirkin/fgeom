{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Math.Statistics
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
-- Simple statistical functions on sets of vectors/numbers
--
-----------------------------------------------------------------------------

module Geometry.Math.Statistics where

import Prelude hiding (foldr,sum)
import Control.Monad (liftM)
import Data.List (sort)
import Data.Foldable

import Geometry.Space
import Geometry.Math.Optimization

-- | Average of elements in Foldable container
mean :: (TensorMath n m, Foldable a, Fractional x)
     => a (Tensor n m x) -> Tensor n m x
mean xs = s /.. n
    where f (v,i) x  = x `seq` v `seq` i `seq` (v.+x,i+1)
          (s, n) = foldl' f (zeros,0) xs

-- | Average of numbers in Foldable container
mean' :: (Foldable a, Fractional x) => a x -> x
mean' xs = s / n
    where f (v,i) x = x `seq` v `seq` i `seq` (v+x,i+1)
          (s, n) = foldl' f (0,0) xs

-- | Two-pass simple algorithm to take covariance matrix for any-dimensional data
var :: (TensorMath n n, TensorMath 1 n, TensorMath n 1, Foldable a, Functor a, Eq x, Fractional x)
    => a (Vector n x) -> Tensor n n x
var xs = (/.. if n == 1 then 1 else n-1) . foldl' (.+) zeros . fmap ((\x -> prodT x x) . (.-me)) $ xs
    where fm (v,i) x = x `seq` v `seq` i `seq` (v.+x,i+1)
          (s, n) = foldl' fm (zeros,0) xs
          me = s /.. n

-- | Two-pass simple algorithm to take variance for one-dimensional data
var' :: (Foldable a, Functor a, Eq x, Fractional x) => a x -> x
var' xs = (/ if n == 1 then 1 else n-1) . sum . fmap ((\x -> x*x) . (+me)) $ xs
    where fm (v,i) x = x `seq` v `seq` i `seq` (v+x,i+1)
          (s, n) = foldl' fm (0,0) xs
          me = - s / n

-- | Calculate median along each dimension  of vectors inside Foldable separately
median :: (TensorMath n m, Foldable a, Traversable a, Ord x) =>
     a (Tensor n m x) -> Tensor n m x
median vs = fmap ((!! n2) . sort . toList) . sequenceA $ vs
    where n2 =  div (foldl' (\i _ -> i+1) 0 vs) 2

-- | Median of the Foldable container of 1D data
median' :: (Foldable a, Ord x) => a x -> x
median' vs = sort xs !! n2
    where n2 =  div (length xs) 2
          xs = toList vs


-- | Planarize 3D set of points
planarize :: ( Foldable a
             , Functor a
             , RealFloat x)
          => a (Vector 3 x) -> Approximately x (Vector 3 x, Vector 3 x, Vector 3 x, a (Vector 2 x))
planarize points = do
    xsm <- isSmall' xd
    ysm <- isSmall' yd
    zsm <- isSmall' zd
    (z,x) <- case (xsm,ysm,zsm) of
        (False,False,False) -> do
            z' <- minn d (Vector2 0 0)
            x' <- maxx d (Vector2 0 0)
            return (z',x')
        ( True, True, True) -> return (Vector3 0 0 1, Vector3 1 0 0)
        (False, True, True) -> return (Vector3 0 0 1, Vector3 1 0 0)
        ( True,False, True) -> return (Vector3 1 0 0, Vector3 0 1 0)
        ( True, True,False) -> return (Vector3 0 1 0, Vector3 0 0 1)
        ( True,False,False) -> return (Vector3 1 0 0, (\(Vector2 a b) -> Vector3 0 a b) $ opt2d vyz yd zd)
        (False, True,False) -> return (Vector3 0 1 0, (\(Vector2 a b) -> Vector3 a 0 b) $ opt2d vxz xd zd)
        (False,False, True) -> return (Vector3 0 0 1, (\(Vector2 a b) -> Vector3 a b 0) $ opt2d vxy xd yd)
--    case (xsm || ysm || zsm, (xsm && ysm) || (ysm && zsm) || (xsm && zsm), xsm && ysm && zsm) of
--        (_,_,True) -> return (Vector3 0 0 1, Vector3 1 0 0)
--        (_,True,False) ->
--    liftM a2x $ minimize (sphericalFunc3d d) (Vector2 0 0)
--    x <- liftM a2x $ minimize (sphericalFunc3d $ neg d) (Vector2 0 0)
    let y = z `cross` x
    return $ (m, x, y, project m x y points)
    where d@(Matrix3x3 xd vxy vxz _ yd vyz _ _ zd) = var points
          m = mean points
          minn d' a0 = liftM a2x $ minimize (sphericalFunc3d d') a0
          maxx d' a0 = liftM a2x $ minimize (sphericalFunc3d $ neg d') a0
          opt2d d12 d11 d22 = Vector2 (cos phi) (sin phi)
            where phi = 0.5 * atan2 (2*d12) (d11-d22)

-- | Project a set of points from 3D to a 2D plane; takes center coordinate, x and y
project :: ( Foldable a
           , Functor a
           , RealFloat x)
        => Vector 3 x -> Vector 3 x -> Vector 3 x -> a (Vector 3 x) -> a (Vector 2 x)
project m x y points = fmap proj points
    where proj p' = let p = p' .- m in Vector2 (x .*. p) (y .*. p)

-- | Direction of the smallest variance in a 3D point set
smallestVarDir3d :: ( Foldable a
                    , Functor a
                    , RealFloat x)
                 => a (Vector 3 x) -> Approximately x (Vector 3 x)
smallestVarDir3d points = liftM a2x $ minimize f (Vector2 0 0)
    where f = sphericalFunc3d $ var points

-- | Direction of the largest variance in a 3D point set
largestVarDir3d :: ( Foldable a
                   , Functor a
                   , RealFloat x)
                => a (Vector 3 x) -> Approximately x (Vector 3 x)
largestVarDir3d points = liftM a2x $ minimize f (Vector2 0 0)
    where f = sphericalFunc3d . neg $ var points


-- HELPERS

a2x :: (Floating x) => Vector 2 x -> Vector 3 x
a2x (Vector2 a b) = Vector3 (sin a * sin b) (cos a * sin b) (cos b)

sphericalFunc3d :: (RealFloat x)
               => Tensor 3 3 x -> NumericFunction 2 2 x
sphericalFunc3d (Matrix3x3 d11 d12 d13 _ d22 d23 _ _ d33) = F2 f df ddf
    where dd12 = d11 - d22
          dd23 = d22 - d33
          f (Vector2 a b) = sa*sa*sb*sb*dd12 + sb*sb*dd23 + 2*(d12*ca*sa*sb*sb + d13*cb*sa*sb + d23*ca*cb*sb)
            where ca = cos a
                  cb = cos b
                  sa = sin a
                  sb = sin b
          df (Vector2 a b) = Vector2 (2*dd12*ca*sa*sb*sb
                                      + 2*(d13*ca*cb*sb - d23*cb*sa*sb + d12*ca*ca*sb*sb - d12*sa*sa*sb*sb))
                                     (2*dd23*cb*sb + 2*dd12*cb*sa*sa*sb
                                      + 2*(d23*ca*cb*cb + d13*cb*cb*sa + 2*d12*ca*cb*sa*sb - d23*ca*sb*sb - d13*sa*sb*sb))
            where ca = cos a
                  cb = cos b
                  sa = sin a
                  sb = sin b
          ddf (Vector2 a b) = Matrix2x2 df20 df11 df11 df02
            where ca = cos a
                  cb = cos b
                  sa = sin a
                  sb = sin b
                  df20 = 2*dd12*ca*ca*sb*sb - 2*dd12*sa*sa*sb*sb
                       + 2*(-d23*ca*cb*sb - d13*cb*sa*sb - 4*d12*ca*sa*sb*sb)
                  df11 = 4*dd12*ca*cb*sa*sb + 2*(d13*ca*cb*cb - d23*cb*cb*sa
                       + 2*d12*ca*ca*cb*sb - 2*d12*cb*sa*sa*sb - d13*ca*sb*sb + d23*sa*sb*sb)
                  df02 = 2*dd23*cb*cb + 2*dd12*cb*cb*sa*sa - 2*dd23*sb*sb - 2*dd12*sa*sa*sb*sb
                       + 2*(2*d12*ca*cb*cb*sa - 4*d23*ca*cb*sb - 4*d13*cb*sa*sb - 2*d12*ca*sa*sb*sb)

testVals2 :: [Vector3 Double]
testVals2 = [ Vector3 0.5427056 3.0 6.532831
            , Vector3 0.9582512 3.0 0.33841032
            , Vector3 3.7996066 3.0 (-5.310967)
            , Vector3 1.5777113 3.0 (-6.4307466)
            , Vector3 (-0.48477972) 3.0 (-6.54336)
            , Vector3 (-0.93017095) 3.0 (-0.21834362)
            , Vector3 (-3.7429376) 3.0 5.211251
            , Vector3 (-1.7203865) 3.0 6.4209256]

testMinDir :: Vector3 Double
testMinDir = Vector3 0.55817 (-0.501373) 0.661114

testMaxDir :: Vector3 Double
testMaxDir = Vector3 0.562248 0.81451 0.143006

testVals :: [Vector3 Double]
testVals =
   [ Vector3  6.8696612336875145 (-6.2613736254440155) (-0.93046195178200852)
   , Vector3  4.5456859960996026 (-4.08305402056822) 4.442305318053406
   , Vector3  (-3.583207953222292) (-8.2455167402029446) 5.543120039575971
   , Vector3  2.9868370196443412 (-6.9265444851633067) 2.2333929213916157
   , Vector3  (-1.1263313322307207) (-7.7213545312960257) 5.6353499429345284
   , Vector3  1.4296792797730815 (-6.261038093281706) 3.4871179910480139
   , Vector3  2.3676186837185367 (-9.0099160055284653) 2.6943119182913953
   , Vector3  3.6719841175047483 (-4.7916061481525958) 2.8008598209791562
   , Vector3  1.3894484600861547 (-9.1357600689131733) 3.255125363268621
   , Vector3  5.5247142806199045 (-0.85825424553870633) 4.9126345459359291
   , Vector3  0.92459183571736814 (-7.9316761782461711) 2.9433784196370549
   , Vector3  2.7323393656303021 (-2.8298328420475922) 4.0093113019196718
   , Vector3  3.4863906806868465 (-6.74214394971537) 1.032803171987162
   , Vector3  2.1904874842320581 (-5.556226208223217) 3.4235020647985479
   , Vector3  3.9698703130798059 (-5.5616724324934559) 2.0525307865078797
   , Vector3  2.7227265474482381 (-10.85170882739491) (-0.17006655832827233)
   , Vector3  0.24659582903006516 (-10.554465701200552) 3.1578480606999313
   , Vector3  (-2.036937538465911) (-8.8876050118414849) 4.2686391025728527
   , Vector3  6.1559401754723266 (-3.7954973455779766) 3.3339808419219823
   , Vector3  0.648229627056327 (-10.532435312540066) 1.2663776277106704
   , Vector3  2.2311193652221109 (-8.1874311289209452) 3.2037433138784333
   , Vector3  3.2592795943879818 (-5.4459103444737558) 2.5029375739881652
   , Vector3  2.2540106067370496 (-8.9723871542847071) 1.8708262388714822
   , Vector3  (-0.023029312842339333) (-6.4824145279783192) 5.699127754006664
   , Vector3  0.95415839956971893 (-9.2403974789117029) 1.9431880425153631
   , Vector3  1.7207375883027716 (-3.2603904866125726) 4.89422755990035
   , Vector3  5.307051722903557 (-6.720227040149898) 3.1224089224140656
   , Vector3  0.075157601046048628 (-8.1034717122154163) 4.66672681325182
   , Vector3  3.7545720326561387 (-8.9287446076566148) 0.23856680844367339
   , Vector3  2.690102320354343 (-8.0333145062512141) 2.63810109903173
   , Vector3  1.4770730485782495 (-8.718087880251252) 1.9574731808978187
   , Vector3  (-0.36346291033146949) (-8.01148646240391) 4.7480511327010611
   , Vector3  1.3646415102642142 (-5.4441761072954717) 5.860428044004804
   , Vector3  1.8061546354323361 (-6.4967445209903394) 1.6897209400157318
   , Vector3  (-1.2856672723548614) (-10.445890678985931) 4.2916302783219908
   , Vector3  2.4297325635995866 (-4.8695681440463492) 5.5310487290904913
   , Vector3  0.16129633677415223 (-6.7206892245739347) 4.4400086172748461
   , Vector3  1.78927163908251 (-6.9052261108989335) 5.0819431644426594
   , Vector3  2.7517958853159952 (-5.1470224408832959) 3.7475440171587513
   , Vector3  (-0.0228679725970502) (-7.7139036320949019) 2.5958928814769067
   , Vector3  1.3554391813973297 (-9.67797380251978) (-0.50168877982946425)
   , Vector3  2.7827641222374484 (-4.7096578779301908) 3.4540693647236091
   , Vector3  (-2.1051058397540992) (-11.723940577154742) 3.7518200766630074
   , Vector3  4.3165125538889795 (-6.4699090574647826) 2.1077175496799225
   , Vector3  7.42125992889139 (-2.27532099261218) 2.5886397499914771
   , Vector3  4.14528475668416 (-5.9574785424724261) 1.5546240077105034
   , Vector3  1.2939123537682382 (-7.8536740403067169) 3.1282882964765832
   , Vector3  2.9584294488637393 (-4.9288931298285013) 1.7992314271794203
   , Vector3  (-0.31653238841101006) (-6.5597301911291463) 3.9801879493472794
   , Vector3  6.1990348222676852 (-4.0744777763233868) 1.0228004585588304
   , Vector3  4.103478992628748 (-4.5993099180016062) 3.5594545789908669
   , Vector3  3.7605586857328008 (-6.11364548961863) 2.328039728686746
   , Vector3  0.041485610522422389 (-8.063789921382579) 2.4370177713011061
   , Vector3  2.715428013821108 (-5.4636704780376526) 3.408952126696247
   , Vector3  0.75161602762374846 (-9.35019271473703) 1.7162296937508281
   , Vector3  1.3036226499359098 (-6.9549646487562313) 2.7704047985454019
   , Vector3  0.72541907853908394 (-6.702716166988381) 4.956524237439127
   , Vector3  (-0.2936100913524422) (-8.6326514845592879) 3.6992003542997
   , Vector3  (-0.032016906503931963) (-4.8357209209219025) 6.0474551187784362
   , Vector3  1.5306552927557173 (-8.70875832587895) 3.2709173887019607
   , Vector3  (-1.7987755767428815) (-11.653837514148288) 3.0124790635125867
   , Vector3  3.3586361907598605 (-9.7468845234720973) 1.0868841208013462
   , Vector3  1.736595017563652 (-5.2159565975962092) 4.953981036319326
   , Vector3  3.5633700331997904 (-3.8496212795664975) 5.0598760956199218
   , Vector3  2.6029526929187985 (-6.6926742862058282) 3.2912930502245716
   , Vector3  3.1052594562265656 (-4.1473301679842169) 5.4526034056352906
   , Vector3  (-1.3163594252045456) (-9.2506135052916658) 3.9224683594486462
   , Vector3  (-0.28138052140917358) (-8.1880151516225972) 3.7762066366557883
   , Vector3  1.0004887703719876 (-7.5521595066485405) 5.1818992301355919
   , Vector3  2.2452040645253883 (-5.8399497961028546) 2.9626750023585693
   , Vector3  4.5239314354171709 (-4.758123597003161) 2.6220565687367703
   , Vector3  1.3516229298085924 (-1.411956137744248) 7.6855136366479435
   , Vector3  4.820913162827285 (-7.2993044860983751) 1.1066088663160982
   , Vector3  3.0630821971674966 (-10.212099986067134) 1.93505510334137
   , Vector3  4.6254048698611747 (-7.3987832795365458) 2.1340854317945013
   , Vector3  2.2838622736898837 (-9.1683056773804683) 1.8608454118820532
   , Vector3  0.53131493416741549 (-9.1249384239588913) 1.8995655351909837
   , Vector3  (-1.312509025855229) (-10.019106231050634) 4.0592957929492632
   , Vector3  2.3476839645942933 (-7.2006358699184352) 1.1577715630306868
   , Vector3  3.8303365030884384 (-4.3497264711379184) 3.5898313864148292
   , Vector3  1.3457530500446926 (-6.6705580370868063) 4.5015528206464008
   , Vector3  0.79076481142314314 (-6.3215279077994735) 4.4320435137330429
   , Vector3  1.309855920334734 (-3.6561517874989407) 6.3608341103537924
   , Vector3  (-0.45205716588187972) (-6.2820499374077627) 7.2557470306927652
   , Vector3  0.89759653937779937 (-10.693396030690719) 2.5784157263115519
   , Vector3  1.5958545128464134 (-12.425800085610545) 0.39268406804102129
   , Vector3  2.1025038285395983 (-4.941964098075605) 4.8252586969743856
   , Vector3  1.8573766078130363 (-11.135781571792997) 1.1421197213749605
   , Vector3  3.3669870509678939 (-5.7601532941723015) 1.3918555250540927
   , Vector3  2.2444417944241155 (-6.7267149055730036) 2.546679585669708
   , Vector3  6.05626185906181 1.1244902703213366 6.3579254564680614
   , Vector3  2.6977064907439807 (-6.59347564031989) 2.7249252268687618
   , Vector3  6.03497073855027 (-4.8835125548878313) 2.1737262235914434
   , Vector3  0.38305121569031964 (-7.77692591965608) 2.5376147369843944
   , Vector3  3.1773949697591619 (-5.5195525756867729) 3.1483503972806521
   , Vector3  1.4180613820222261 (-7.3120895294111836) 3.854296264164474
   , Vector3  3.3419594458833632 (-7.2597655370084633) 0.4832262429091978
   , Vector3  3.3280696330011477 (-8.6161121855379079) 1.5473310891705043
   , Vector3  (-2.8880929263887758) (-10.217056792841289) 4.1856990306686193
   , Vector3  (-0.9673586914027883) (-12.315966043832525) 0.86244540283186444]
