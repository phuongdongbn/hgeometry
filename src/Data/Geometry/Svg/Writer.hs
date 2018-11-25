{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Geometry.Svg.Writer where

import           Control.Lens
import qualified Data.ByteString.Lazy as B
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Ipe.Types
import qualified Data.Geometry.Ipe.Types as Ipe
import           Data.Geometry.Ipe.Writer (IpeWriteText(..))
import           Data.Geometry.Point
import           Data.Geometry.PolyLine
import           Data.Geometry.Polygon
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Monoid (mconcat)
import           Data.Semigroup.Foldable (toNonEmpty)
import           Data.Text (Text)
import           Text.Blaze (ToMarkup(toMarkup), ToValue(toValue))
import qualified Text.Blaze.Svg as Svg
import qualified Text.Blaze.Svg.Renderer.Utf8 as SvgRender
import           Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as Svg
import qualified Text.Blaze.Svg11.Attributes as A
--------------------------------------------------------------------------------




svgO :: ToMarkup a => a -> Svg.Svg
svgO = Svg.toSvg


toSvgXML :: ToMarkup t => t -> B.ByteString
toSvgXML = SvgRender.renderSvg . Svg.toSvg

-- instance Coordinate r => ToValue r where


instance ToValue r => ToMarkup (TextLabel r) where
  toMarkup (Label t p) = Svg.text t ! A.x (toValue $ p^.xCoord)
                                    ! A.y (toValue $ p^.yCoord)

-- instance IpeWriteText t => ToValue t where
--   toValue = undefined


-- toValueDef   :: IpeWriteText t => t -> Value
-- toValueDef x = case ipeWriteText x of
--                   Nothing -> mempty
--                   Just t  -> toValue t



instance Show r => ToValue (PathSegment r) where
  toValue = \case
    PolyLineSegment pl -> Svg.mkPath . toPath $ pl^.points.to toNonEmpty
    PolygonPath  pg    -> Svg.mkPath $ do toPath $ pg^.outerBoundary.to toNonEmpty
                                          Svg.z
    EllipseSegment m   -> undefined
    _                  -> error "toValeue: not implemented yet"

toPath     :: Show r => NonEmpty (Point 2 r :+ p) -> Svg.Path
toPath pts = case (^.core) <$> pts of
               (v:|vs) -> do Svg.l (v^.xCoord) (v^.yCoord)
                             mapM_ (\(Point2 x y) -> Svg.m x y) vs

1
instance Show r => ToMarkup (Ipe.Path r) where
  toMarkup (Path s) = let pa = mconcat . map toValue . F.toList $ s
                      in Svg.path ! A.d pa
