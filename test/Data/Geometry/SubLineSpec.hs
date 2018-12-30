{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.SubLineSpec where

import Control.Lens
import Data.Ext
import Data.Fixed
import Data.Geometry
import Data.Geometry.Line
import Data.Geometry.LineSegment
import Data.Geometry.SubLine
import Data.Proxy
import Data.Ratio
import Data.UnBounded
import Data.Vinyl.CoRec
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.HGeometryInstances ()


import Debug.Trace

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "subLineTests" $ do
  spec' ("Rational", Proxy :: Proxy Rational)
  -- spec' ("Pico", Proxy :: Proxy Pico)

  it "Intersection test" $
    let mySeg :: LineSegment 2 () Rational
        mySeg    = ClosedLineSegment (ext origin) (ext $ Point2 14 0)
        myLine :: SubLine 2 () (UnBounded Rational) Rational
        myLine   = fromLine $ lineThrough (Point2 0 0) (Point2 10 0)
        myAnswer :: Interval () (UnBounded Rational)
        myAnswer = ClosedInterval (ext $ Val 0) (ext . Val $ 7 % 5)
    in (myLine `intersect` (mkSL mySeg))
       `shouldBe`
       coRec (myLine&subRange .~ myAnswer)


spec'       :: forall proxy r. (Ord r, Fractional r, Arbitrary r, Show r)
            => (String, proxy r) -> Spec
spec' (t,_) = describe ("subline tests with type " <> t) $ do
    it "subline specialization in R^2" $
      property $ \(alpha :: r) l@(Line p v) (i :: Interval () r)  ->
        let q  = p .+^ alpha *^ v
            sl = SubLine l i
        in onSubLine2 q sl `shouldBe` onSubLineOrig q sl

    it "onSubLine: unBounded subline specialization in R^2" $
      property $ \(alpha :: r) l@(Line p v) (i :: Interval () (UnBounded r))  ->
        let q  = p .+^ alpha *^ v
            sl = SubLine l i
        in onSubLine2UB q sl `shouldBe` onSubLineUB q sl

    it "onSubLine: for bounded unbounded is the same as bounded (R^2)" $
      property $ \(alpha :: r) l@(Line p v) (i :: Interval () r)  ->
        let q  = p .+^ alpha *^ v
            sl = SubLine l i
        in onSubLine2UB q (SubLine l (Val <$> i)) `shouldBe` onSubLine2 q sl

    it "onSubLine: for bounded unbounded is the same as bounded" $
      property $ \(alpha :: r) l@(Line p v) (i :: Interval () r)  ->
        let q  = p .+^ alpha *^ v
            sl = SubLine l i
        in onSubLineUB q (SubLine l (Val <$> i)) `shouldBe` onSubLine q sl

    it "manual test " $
        ((Point2 (-1) (-1 :: r)) `onSubLine2`
         (seg^._SubLine))
      `shouldBe` False


-- mySpec = it "onSubLine: unBounded subline specialization in R^2" $
--     property $
--       \(alpha :: Rational)
--        l@(Line p v)
--        (i :: Interval () (UnBounded Rational))  ->
--         let q  = origin .+^ alpha *^ v
--             sl = SubLine (Line origin v) i
--         in (traceShow (q,sl) $ onSubLine2UB1 q sl) `shouldBe` onSubLineUB q sl

-- mySpecP = it "onSubLine: unBounded subline specialization in R^2" $
--     property $
--       \(alpha :: Pico)
--        l@(Line p v)
--        (i :: Interval () (UnBounded Pico))  ->
--         let q  = origin .+^ alpha *^ v
--             sl = SubLine (Line origin v) i
--         in (traceShow (q,sl) $ onSubLineUB q sl) `shouldBe` onSubLineUB q sl

mkSL  :: (Num r, Arity d) => LineSegment d () r -> SubLine d () (UnBounded r) r
mkSL s = s^._SubLine.re _unBounded


seg :: Num r => LineSegment 2 () r
seg = ClosedLineSegment (ext (Point2 1 1)) (ext (Point2 5 5))



-- | Original def of onSubline
onSubLineOrig                 :: (Ord r, Fractional r, Arity d)
                          => Point d r -> SubLine d p r r -> Bool
onSubLineOrig p (SubLine l r) = toOffset' p l `inInterval` r






-- | given point p, and a Subline l r such that p lies on line l, test if it
-- lies on the subline, i.e. in the interval r
-- onSubLine2UB                   :: (Ord r, Fractional r)
--
--
onSubLine2UB1                   :: (Ord r, Fractional r, Show r, Show p)
                               => Point 2 r -> SubLine 2 p (UnBounded r) r -> Bool
p `onSubLine2UB1` sl | traceShow ("IN: ", p,sl)                                 False = undefined
p `onSubLine2UB1` sl = traceShowId $ lambda `inInterval` srsq
  where
    dist = squaredEuclideanDist (sl^.line.anchorPoint)

    srsq = traceShowId $
      fmap (\alpha -> signum alpha * dist (pointAt alpha $ sl^.line)) <$> sl^.subRange
    lambda = traceShow ("det",detSign) $ Val $ detSign * dist p

    detSign = case ccw b a p of
                CCW      -> 1
                CW       -> (-1)
                CoLinear -> (-1) -- if p is colinear with the line through a and b
                                -- and it lies *on* the line sl^.line, then a=p
                                -- so the distance must be zero anyway.

    a = sl^.line.anchorPoint
    -- the vector from a to the anchor point o is per
    b = a .+^ sl^.line.to perpendicularTo.direction

    -- the main idea is as follows; we square the values determining the
    -- subrange as well as the distance from p to the anchorpoint; hence we get
    -- the 'squared' lambda value corresponding to p. The original point lies
    -- in the range iff the squared lambda value lies in squared values range.
    -- We have to be a bit careful about the signs of the values when we square
    -- them. Moreover, to determine if p lies in the "positive" half-line
    -- starting from our anchor a we test if it lies in the left half-plane of
    -- the line perpendicular to a.
