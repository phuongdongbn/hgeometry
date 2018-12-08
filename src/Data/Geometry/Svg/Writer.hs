{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.Svg.Writer where

import           Control.Lens hiding (rmap, Const(..))
import qualified Data.ByteString.Lazy as B
import           Data.Ext
import qualified Data.Foldable as F
import qualified Data.Geometry.Ipe.Attributes as IA
import           Data.Geometry.Ipe.Types
import           Data.Geometry.Ipe.Color(IpeColor(..))
import           Data.Geometry.Ipe.Value(IpeValue(..))
import qualified Data.Geometry.Ipe.Types as Ipe
import           Data.Geometry.Ipe.Writer (IpeWriteText(..))
import           Data.Geometry.Point
import           Data.Geometry.PolyLine
import           Data.Geometry.Polygon
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import           Data.Monoid (mconcat)
import           Data.Proxy
import           Data.Semigroup.Foldable (toNonEmpty)
import           Data.Text (Text)
import           Data.Vinyl hiding (Label)
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel
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

instance Show r => ToMarkup (IpeObject r) where
  toMarkup (IpeGroup g)     = toMarkup g
  -- toMarkup (IpeImage i)     = toMarkup i
  toMarkup (IpeTextLabel t) = toMarkup t
  -- toMarkup (IpeMiniPage m)  = toMarkup m
  toMarkup (IpeUse u)       = toMarkup u
  toMarkup (IpePath p)      = toMarkup p


instance ( ToMarkup g
         , IA.AllSatisfy IpeToSvgAttr rs
         , RecAll (IA.Attr f) rs ToValue
         ) => ToMarkup (g :+ IA.Attributes f rs) where
  toMarkup (i :+ ats) = toMarkup i `applyAts` svgWriteAttrs ats

instance Show r => ToMarkup (TextLabel r) where
  toMarkup (Label t p) = Svg.text t ! A.x (toAValue $ p^.xCoord)
                                    ! A.y (toAValue $ p^.yCoord)

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

instance Show r => ToMarkup (Ipe.Path r) where
  toMarkup (Path s) = let pa = mconcat . map toValue . F.toList $ s
                      in Svg.path ! A.d pa

instance Show r => ToMarkup (Ipe.IpeSymbol r) where
  toMarkup (Symbol p _) = Svg.circle ! A.cx (toAValue $ p^.xCoord)
                                     ! A.cy (toAValue $ p^.yCoord)
                                     ! A.r  (toAValue 5)
    -- TODO: for now just draw a disk of fixed radius

instance Show r => ToMarkup (Ipe.Group r) where
  toMarkup (Group os) = Svg.g (mapM_ toMarkup os)

--------------------------------------------------------------------------------
-- * Dealing with attributes

applyAts    :: Svg.Markup -> [(SvgF, Svg.AttributeValue)] -> Svg.Markup
applyAts x0 = F.foldl' (\x (f,v) -> x ! f v) x0

-- | Functon to write all attributes in a Rec
svgWriteAttrs              :: ( IA.AllSatisfy IpeToSvgAttr rs
                              , RecAll (IA.Attr f) rs ToValue
                              )
                           => IA.Attributes f rs
                           -> [(SvgF, Svg.AttributeValue)]
svgWriteAttrs (IA.Attrs r) = catMaybes . recordToList $ IA.zipRecsWith f (writeAttrFunctions r)
                                                                         (writeAttrValues r)
  where
    f (Const mn) (Const mv) = Const $ (,) <$> mn <*> mv

-- | Writing Attribute names
writeAttrFunctions           :: IA.AllSatisfy IpeToSvgAttr rs => Rec f rs
                             -> Rec (Const (Maybe SvgF)) rs
writeAttrFunctions RNil      = RNil
writeAttrFunctions (x :& xs) = Const (write'' x) :& writeAttrFunctions xs
  where
    write''   :: forall f s. IpeToSvgAttr s => f s -> Maybe SvgF
    write'' _ = attrSvg (Proxy :: Proxy s)


-- | Writing the attribute values
writeAttrValues :: RecAll f rs ToValue => Rec f rs -> Rec (Const (Maybe Svg.AttributeValue)) rs
writeAttrValues = rmap (\(Compose (Dict x)) -> Const . Just $ toValue x)
                . reifyConstraint (Proxy :: Proxy ToValue)


type SvgF = Svg.AttributeValue -> Svg.Attribute

-- | For the types representing attribute values we can get the name/key to use
-- when serializing to ipe.
class IpeToSvgAttr (a :: IA.AttributeUniverse) where
  attrSvg :: Proxy a -> Maybe SvgF

-- CommonAttributeUnivers
instance IpeToSvgAttr IA.Layer           where attrSvg _ = Nothing
instance IpeToSvgAttr IA.Matrix          where attrSvg _ = Nothing -- TODO
instance IpeToSvgAttr IA.Pin             where attrSvg _ = Nothing
instance IpeToSvgAttr IA.Transformations where attrSvg _ = Nothing

-- IpeSymbolAttributeUniversre
instance IpeToSvgAttr IA.Stroke       where attrSvg _ = Just A.stroke
instance IpeToSvgAttr IA.Fill         where attrSvg _ = Just A.fill
instance IpeToSvgAttr IA.Pen          where attrSvg _ = Nothing
instance IpeToSvgAttr IA.Size         where attrSvg _ = Nothing

-- PathAttributeUniverse
instance IpeToSvgAttr IA.Dash       where attrSvg _ = Nothing
instance IpeToSvgAttr IA.LineCap    where attrSvg _ = Just A.strokeLinecap
instance IpeToSvgAttr IA.LineJoin   where attrSvg _ = Nothing
instance IpeToSvgAttr IA.FillRule   where attrSvg _ = Nothing
instance IpeToSvgAttr IA.Arrow      where attrSvg _ = Nothing
instance IpeToSvgAttr IA.RArrow     where attrSvg _ = Nothing
instance IpeToSvgAttr IA.Opacity    where attrSvg _ = Just A.opacity
instance IpeToSvgAttr IA.Tiling     where attrSvg _ = Nothing
instance IpeToSvgAttr IA.Gradient   where attrSvg _ = Nothing

-- GroupAttributeUniverse
instance IpeToSvgAttr IA.Clip     where attrSvg _ = Just A.clip


--------------------------------------------------------------------------------

instance Show r => ToValue (IpeColor r) where
  toValue (IpeColor v) = case v of
                           Named t  -> toAValue t
                           Valued v -> toAValue v


--------------------------------------------------------------------------------

toAValue :: Show a => a -> Svg.AttributeValue
toAValue = toValue . show
