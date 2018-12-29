{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.Polygon.RegressionSpec where

import Data.Ext
import Data.Geometry
import Data.Geometry.Polygon
import Data.Geometry.SubLine
import Data.UnBounded
import Data.Geometry.Ipe
import Data.Vinyl.CoRec
import Data.Fixed
import Test.Hspec
import Test.QuickCheck.HGeometryInstances ()

type Point2  = Point 2
type Polygon' = SimplePolygon ()

polygonFromPoints :: [Point2 r] -> Polygon' r
polygonFromPoints = fromPoints . fmap ext

testArea ::  Fractional r => [Point2 r]
testArea =
  [ Point2 5584390.945938013 2284567.4635945037
  , Point2 5562410.061516319 2285869.7979417136
  , Point2 5563196.65161862  2250738.663576637
  , Point2 5579688.373487147 2252038.6420285213
  ]

polygon :: Fractional r => Polygon' r
polygon = polygonFromPoints testArea

insidePoint, outsidePoint :: Fractional r => Point2 r
insidePoint  = Point2 5565974.538888888 2273030.9266712796
outsidePoint = Point2 5814191.399840455 2393283.2821864313

spec :: Spec
spec =
  describe "insidePolygon" $ do
    it "describes possible regression" $ do
      (insidePoint `insidePolygon` (polygon :: Polygon' Double)) `shouldBe` True
      (outsidePoint `insidePolygon` (polygon :: Polygon' Double)) `shouldBe` False
    it "describes possible regression as well" $ do
      ((transfD insidePoint) `insidePolygon` polygonD) `shouldBe` True
      ((transfD outsidePoint) `insidePolygon` polygonD) `shouldBe` False

    it "does not describe possible regression" $ do
      (insidePoint `insidePolygon` (polygon :: Polygon' Rational)) `shouldBe` True
      (outsidePoint `insidePolygon` (polygon :: Polygon' Rational)) `shouldBe` False
    it "does not describe possible regression either" $ do
      ((transfR insidePoint) `insidePolygon` polygonR) `shouldBe` True
      ((transfR outsidePoint) `insidePolygon` polygonR) `shouldBe` False

    it "does not describe possible regression either" $ do
      (insidePoint `insidePolygon` (polygon :: Polygon' Pico)) `shouldBe` True
      (outsidePoint `insidePolygon` (polygon :: Polygon' Pico)) `shouldBe` False


polygonD :: Polygon' Double
polygonD = transfD $ polygon

polygonR :: Polygon' Rational
polygonR = transfR $ polygon

-- polygon'' = translate

tran :: Fractional r => Vector 2 r
tran = (Vector2 (-5584390) (-2284567))
scal :: Fractional r => r
scal =  1/5000

transfD :: (IsTransformable g, NumType g ~ Double, Dimension g ~ 2) => g -> g
transfD = scaleUniformlyBy scal . translateBy tran

transfR :: (IsTransformable g, NumType g ~ Rational, Dimension g ~ 2) => g -> g
transfR = scaleUniformlyBy scal . translateBy tran
