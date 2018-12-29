{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.Polygon.RegressionSpec where

import Data.Ext
import Data.Geometry
import Data.Geometry.Polygon
import Data.Vinyl.CoRec
import Test.Hspec
import Test.QuickCheck.HGeometryInstances ()

type Point2  = Point 2 Double
type Polygon' = SimplePolygon () Double

polygonFromPoints :: [Point2] -> Polygon'
polygonFromPoints = fromPoints . fmap ext

testArea :: [Point2]
testArea =
  [ Point2 5584390.945938013 2284567.4635945037
  , Point2 5562410.061516319 2285869.7979417136
  , Point2 5563196.65161862  2250738.663576637
  , Point2 5579688.373487147 2252038.6420285213
  ]

polygon :: Polygon'
polygon = polygonFromPoints testArea

insidePoint, outsidePoint :: Point2
insidePoint  = Point2 5565974.538888888 2273030.9266712796
outsidePoint = Point2 5814191.399840455 2393283.2821864313

spec :: Spec
spec =
  describe "insidePolygon" $ do
    it "describes possible regression" $ do
      (insidePoint `insidePolygon` polygon) `shouldBe` True
      (outsidePoint `insidePolygon` polygon) `shouldBe` False
