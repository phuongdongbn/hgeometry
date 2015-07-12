{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Ipe.IpeOut where

import           Control.Applicative
import           Control.Lens hiding (only)
import           Data.Bifunctor
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Ball hiding (disk)
import           Data.Geometry.Boundary
import           Data.Geometry.Ipe.Attributes
import           Data.Geometry.Ipe.Types
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.PolyLine
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import qualified Data.List.NonEmpty as NE
import           Data.Monoid
import           Data.Proxy
import qualified Data.Seq2 as S2
import           Data.Text(Text)
import qualified Data.Traversable as Tr
import           Data.Vinyl

--------------------------------------------------------------------------------

newtype IpeOut g i = IpeOut { asIpe :: g -> i }

-- Given an geometry object, and a record with its attributes, construct an ipe
-- Object representing it using the default conversion.
asIpeObject :: (HasDefaultIpeOut g, DefaultIpeOut g ~ i, NumType g ~ r)
            => g -> IpeAttributes i r -> IpeObject r
asIpeObject = asIpeObject' defaultIpeOut

-- -- | Given a IpeOut that specifies how to convert a geometry object into an
-- ipe geometry object, the geometry object, and a record with its attributes,
-- construct an ipe Object representing it.
asIpeObject'          :: (ToObject i, NumType g ~ r)
                      => IpeOut g (i r) -> g -> IpeAttributes i r -> IpeObject r
asIpeObject' io g ats = asIpe (ipeObject io ats) g

--------------------------------------------------------------------------------

-- | Helper to construct an IpeOut g IpeObject , if we already know how to
-- construct a specific Ipe type.
ipeObject        :: (ToObject i, NumType g ~ r)
                 => IpeOut g (i r) -> IpeAttributes i r -> IpeOut g (IpeObject r)
ipeObject io ats = IpeOut $ flip ipeObject' ats . asIpe io

-- | Construct an ipe object from the core of an Ext
coreOut    :: IpeOut g i -> IpeOut (g :+ a) i
coreOut io = IpeOut $ asIpe io . (^.core)

--------------------------------------------------------------------------------
-- * Default Conversions

class ToObject (DefaultIpeOut g) => HasDefaultIpeOut g where
  type DefaultIpeOut g :: * -> *
  defaultIpeOut :: IpeOut g ((DefaultIpeOut g) (NumType g))

instance HasDefaultIpeOut (Point 2 r) where
  type DefaultIpeOut (Point 2 r) = IpeSymbol
  defaultIpeOut = diskMark

instance HasDefaultIpeOut (LineSegment 2 p r) where
  type DefaultIpeOut (LineSegment 2 p r) = Path
  defaultIpeOut = lineSegment

instance Floating r => HasDefaultIpeOut (Disk p r) where
  type DefaultIpeOut (Disk p r) = Path
  defaultIpeOut = disk

--------------------------------------------------------------------------------
-- * Point Converters

mark   :: Text -> IpeOut (Point 2 r) (IpeSymbol r)
mark n = IpeOut $ flip Symbol n

diskMark :: IpeOut (Point 2 r) (IpeSymbol r)
diskMark = mark "mark/disk(sx)"



--------------------------------------------------------------------------------

lineSegment :: IpeOut (LineSegment 2 p r) (Path r)
lineSegment = fromPathSegment lineSegment'

lineSegment' :: IpeOut (LineSegment 2 p r) (PathSegment r)
lineSegment' = IpeOut $ PolyLineSegment . fromLineSegment . first (const ())


polyLine :: IpeOut (PolyLine 2 p r) (Path r)
polyLine = fromPathSegment polyLine'

polyLine' :: IpeOut (PolyLine 2 a r) (PathSegment r)
polyLine' = IpeOut $ PolyLineSegment . first (const ())


disk :: Floating r => IpeOut (Disk p r) (Path r)
disk = IpeOut $ asIpe circle . Boundary

circle :: Floating r => IpeOut (Circle p r) (Path r)
circle = fromPathSegment circle'

circle' :: Floating r => IpeOut (Circle p r) (PathSegment r)
circle' = IpeOut circle''
  where
    circle'' (Circle (c :+ _) r) = EllipseSegment m
      where
        m = translation (toVec c) |.| uniformScaling (sqrt r) ^. transformationMatrix
        -- m is the matrix s.t. if we apply m to the unit circle centered at the origin, we
        -- get the input circle.


-- | Helper to construct a IpeOut g Path, for when we already have an IpeOut g PathSegment
fromPathSegment    :: IpeOut g (PathSegment r) -> IpeOut g (Path r)
fromPathSegment io = IpeOut $ Path . S2.l1Singleton . asIpe io




ls = (ClosedLineSegment (only origin) (only (point2 1 1)))


test :: IpeObject Integer
test = asIpeObject' lineSegment ls $ mempty <> attr SStroke (IpeColor "red")




-- test' :: Attributes (PathAttrElfSym1 Integer) (IpeObjectAttrF (Path Integer) (PathAttrElfSym1 Integer))
-- -- test' :: RecApplicative (IpeObjectAttrF (Path Integer) (IpeObjectSymbolF (Path Integer)))
-- --       => IpeAttributes (Path Integer)
-- test' = mempty




-- -- test' :: IpeObject Integer ('IpePath '[])
-- test' = asIpeObject' ls emptyPathAttributes




-- emptyPathAttributes :: Rec (PathAttribute r) '[]
-- emptyPathAttributes = RNil