module Data.Geometry.Svg where

import           Control.Lens
import           Data.Ext
import           Data.Geometry.Ipe.Types
import           Data.Geometry.Point
import           Text.Blaze.Svg
import qualified Text.Blaze.Svg11.Attributes as A



svgO = toSvg


-- instance Coordinate r => ToValue r where


instance ToValue r => ToMarkup (TextLabel r) where
  toMarkup (Label t p) = text t ! A.x (toValue $ p^.xCoord)
                                ! A.y (toValue $ p^.yCoord)
