{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Geometry.SubLine where

import           Control.Lens
import           Data.Bifunctor
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Interval
import           Data.Geometry.Line.Internal
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import           Data.Ratio
import qualified Data.Traversable as T
import           Data.UnBounded
import           Data.Util
import           Data.Vinyl
import           Data.Vinyl.CoRec

--------------------------------------------------------------------------------

-- | Part of a line. The interval is ranged based on the vector of the
-- line l, and s.t.t zero is the anchorPoint of l.
data SubLine d p s r = SubLine { _line     :: Line d r
                               , _subRange :: Interval p s
                               }
makeLenses ''SubLine

type instance Dimension (SubLine d p s r) = d


deriving instance (Show r, Show s, Show p, Arity d) => Show (SubLine d p s r)
-- deriving instance (Read r, Read p, Arity d) => Read (SubLine d p r)
deriving instance (Eq r, Eq s, Fractional r, Eq p, Arity d)     => Eq (SubLine d p s r)
deriving instance Arity d                   => Functor (SubLine d p s)
deriving instance Arity d                   => F.Foldable (SubLine d p s)
deriving instance Arity d                   => T.Traversable (SubLine d p s)


-- | Annotate the subRange with the actual ending points
fixEndPoints    :: (Num r, Arity d) => SubLine d p r r -> SubLine d (Point d r :+ p) r r
fixEndPoints sl = sl&subRange %~ f
  where
    ptAt              = flip pointAt (sl^.line)
    label (c :+ e)    = (c :+ (ptAt c :+ e))
    f ~(Interval l u) = Interval (l&unEndPoint %~ label)
                                 (u&unEndPoint %~ label)

-- | Annotate the subRange with the actual ending points (if they exist)
fixEndPointsUB    :: forall d p r. (Num r, Arity d)
                  => SubLine d p                            (UnBounded r) r
                  -> SubLine d (UnBounded (Point d r) :+ p) (UnBounded r) r
fixEndPointsUB sl = sl&subRange %~ f
  where
    ptAt            :: r -> Point d r
    ptAt            = flip pointAt (sl^.line)
    label           :: UnBounded r :+ p -> UnBounded r :+ (UnBounded (Point d r) :+ p)
    label (mc :+ e) = let mp = ptAt <$> mc in mc :+ (mp :+ e)
    f                 :: Interval p                            (UnBounded r)
                      -> Interval (UnBounded (Point d r) :+ p) (UnBounded r)
    f ~(Interval l u) = Interval (l&unEndPoint %~ label)
                                 (u&unEndPoint %~ label)

-- | forget the extra information stored at the endpoints of the subline.
dropExtra :: SubLine d p s r -> SubLine d () s r
dropExtra = over subRange (first (const ()))

_unBounded :: Prism' (SubLine d p (UnBounded r) r) (SubLine d p r r)
_unBounded = prism' toUnbounded fromUnbounded

-- | Transform into an subline with a potentially unbounded interval
toUnbounded :: SubLine d p r r -> SubLine d p (UnBounded r) r
toUnbounded = over subRange (fmap Val)

-- | Try to make a potentially unbounded subline into a bounded one.
fromUnbounded               :: SubLine d p (UnBounded r) r -> Maybe (SubLine d p r r)
fromUnbounded (SubLine l i) = SubLine l <$> mapM unBoundedToMaybe i

-- | given point p, and a Subline l r such that p lies on line l, test if it
-- lies on the subline, i.e. in the interval r
onSubLine                 :: (Ord r, Fractional r, Arity d)
                          => Point d r -> SubLine d p r r -> Bool
onSubLine p (SubLine l r) = case toOffset p l of
                              Nothing -> False
                              Just x  -> x `inInterval` r

-- | given point p, and a Subline l r such that p lies on line l, test if it
-- lies on the subline, i.e. in the interval r
onSubLineUB                   :: (Ord r, Fractional r)
                              => Point 2 r -> SubLine 2 p (UnBounded r) r -> Bool
p `onSubLineUB` (SubLine l r) = case toOffset p l of
                                  Nothing -> False
                                  Just x  -> Val x `inInterval` r

-- | given point p, and a Subline l r such that p lies on line l, test if it
-- lies on the subline, i.e. in the interval r
onSubLine2        :: (Ord r, Num r) => Point 2 r -> SubLine 2 p r r -> Bool
p `onSubLine2` sl = d `inInterval` r
  where
    -- get the endpoints (a,b) of the subline
    SubLine _ (Interval s e) = fixEndPoints sl
    a = s^.unEndPoint.extra.core
    b = e^.unEndPoint.extra.core
    d = (p .-. a) `dot` (b .-. a)
    -- map to an interval corresponding to the length of the segment
    r = Interval (s&unEndPoint.core .~ 0) (e&unEndPoint.core .~ squaredEuclideanDist b a)



-- | given point p, and a Subline l r such that p lies on line l, test if it
-- lies on the subline, i.e. in the interval r
-- onSubLine2UB                   :: (Ord r, Fractional r)
--                                => Point 2 r -> SubLine 2 p (UnBounded r) r -> Bool
-- p `onSubLine2UB` (SubLine m i) = case (i^.start.core, i^.end.core) of
--     (MinInfinity,MinInfinity) -> False
--     (MinInfinity,Val u)       -> onSubLine2UB1 p u (m&direction %~ ((-1)*^))
--     (MinInfinity,MaxInfinity) -> True
--     (Val _, MinInfinity)      -> False  -- this is an invalid interval to begin with
--     (Val l, MaxInfinity)      -> onSubLine2UB1 p l m
--     (Val l, Val u)            -> p `onSubLine2` (SubLine m $ mkI l u i)
--     (MaxInfinity,_)           -> False
--   where
--     mkI l u (Interval s e) = Interval (s&unEndPoint.core .~ l) (e&unEndPoint.core .~ u)

-- -- | given p and a subline l that is bounded on at least one side, with p on
-- -- the supporting line of sl, test if p lies *on* sl.
-- -- onSubLine2UB1        :: (Ord r, Fractional r)
-- --                      => Point 2 r -> r -> Line 2 r -> Bool
-- onSubLine2UB1       :: (Ord r, Fractional r) => Point 2 r -> r -> Line 2 r -> Bool
-- onSubLine2UB1 p l m = undefined
--   where
--     d = (p .-. a) `dot` (b .-. a)

-- onSubLine2UB                       :: (Ord r, Fractional r)
--                                    => Point 2 r -> SubLine 2 p (UnBounded r) r -> Bool
-- p `onSubLine2UB` (dropExtra -> sl) = any' (maybe False check) $ split 0 (sl^.subRange)
--   where
--     any' f (Two a b) = f a || f b
--     check i = p `onSubLine2UB1` (sl&subRange .~ i)

-- split     :: forall r. Ord r
--           => r -> Interval () (UnBounded r) -> Two (Maybe (Interval () (UnBounded r)))
-- split x i = Two (f $ Interval (Open   $ ext MinInfinity) (Closed $ ext (Val x)))
--                 (f $ Interval (Closed $ ext (Val x))     (Open   $ ext MaxInfinity) )
--   where
--     f j = asA @(Interval () (UnBounded r)) $ j `intersect` i

-- | given point p, and a Subline l r such that p lies on line l, test if it
-- lies on the subline, i.e. in the interval r
-- onSubLine2UB                   :: (Ord r, Fractional r)
--
--
onSubLine2UB                   :: (Ord r, Fractional r)
                               => Point 2 r -> SubLine 2 p (UnBounded r) r -> Bool
p `onSubLine2UB` sl = lambda `inInterval` srsq
  where
    dist = squaredEuclideanDist (sl^.line.anchorPoint)

    srsq = fmap (\alpha -> signum alpha * dist (pointAt alpha $ sl^.line)) <$> sl^.subRange
    lambda = Val $ detSign * dist p

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

testL :: SubLine 2 () (UnBounded Rational) Rational
testL = SubLine (Line origin $ Vector2 10 5) (Interval (Closed $ ext (Val 1)) (Open $ ext MaxInfinity))

testP = pointAt 5 (testL^.line)


type instance IntersectionOf (SubLine 2 p s r) (SubLine 2 q s r) = [ NoIntersection
                                                                   , Point 2 r
                                                                   , SubLine 2 p s r
                                                                   ]

instance (Ord r, Fractional r) =>
         (SubLine 2 p r r) `IsIntersectableWith` (SubLine 2 p r r) where

  nonEmptyIntersection = defaultNonEmptyIntersection

  sl@(SubLine l r) `intersect` sm@(SubLine m _) = match (l `intersect` m) $
         (H $ \NoIntersection -> coRec NoIntersection)
      :& (H $ \p@(Point _)    -> if onSubLine2 p sl && onSubLine2 p sm
                                 then coRec p
                                 else coRec NoIntersection)
      :& (H $ \_             -> match (r `intersect` s'') $
                                      (H $ \NoIntersection -> coRec NoIntersection)
                                   :& (H $ \i              -> coRec $ SubLine l i)
                                   :& RNil
           )
      :& RNil
    where
      s'  = (fixEndPoints sm)^.subRange
      s'' = bimap (^.extra) id
          $ s'&start.core .~ toOffset' (s'^.start.extra.core) l
              &end.core   .~ toOffset' (s'^.end.extra.core)   l

instance (Ord r, Fractional r) =>
         (SubLine 2 p (UnBounded r) r) `IsIntersectableWith` (SubLine 2 p (UnBounded r) r) where
  nonEmptyIntersection = defaultNonEmptyIntersection

  sl@(SubLine l r) `intersect` sm@(SubLine m _) = match (l `intersect` m) $
         (H $ \NoIntersection -> coRec NoIntersection)
      :& (H $ \p@(Point _)    -> if onSubLine2UB p sl && onSubLine2UB p sm
                                 then coRec p
                                 else coRec NoIntersection)
      :& (H $ \_             -> match (r `intersect` s'') $
                                      (H $ \NoIntersection -> coRec NoIntersection)
                                   :& (H $ \i              -> coRec $ SubLine l i)
                                   :& RNil
           )
      :& RNil
    where
      -- convert to points, then convert back to 'r' values (but now w.r.t. l)
      s'  = getEndPointsUnBounded sm
      s'' = second (fmap f) s'
      f = flip toOffset' l

-- | Get the endpoints of an unbounded interval
getEndPointsUnBounded    :: (Num r, Arity d) => SubLine d p (UnBounded r) r
                         -> Interval p (UnBounded (Point d r))
getEndPointsUnBounded sl = second (fmap f) $ sl^.subRange
  where
    f = flip pointAt (sl^.line)

fromLine   :: Arity d => Line d r -> SubLine d () (UnBounded r) r
fromLine l = SubLine l (ClosedInterval (ext MinInfinity) (ext MaxInfinity))


-- testL :: SubLine 2 () (UnBounded Rational)
-- testL = SubLine (horizontalLine 0) (Interval (Closed (only 0)) (Open $ only 10))

-- horL :: SubLine 2 () (UnBounded Rational)
-- horL = fromLine $ horizontalLine 0


-- test = (testL^.subRange) `intersect` (horL^.subRange)

-- toOffset (Point2 minInfinity minInfinity) (horizontalLine 0)
-- testzz = let f  = bimap (fmap Val) (const ())
--          in

testz :: SubLine 2 () Rational Rational
testz = SubLine (Line (Point2 0 0) (Vector2 10 0))
                (Interval (Closed (0 % 1 :+ ())) (Closed (1 % 1 :+ ())))
