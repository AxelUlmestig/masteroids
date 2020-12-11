module Properties.Physics (physicsProperties) where

import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      (Arbitrary, Gen,
                                                       arbitrary, suchThat)

import           Physics                              (Position, Velocity, absV,
                                                       addV, bounce, createV,
                                                       scaleV, subtractV)

physicsProperties :: Test
physicsProperties = testGroup "properties" [
    testProperty "momentum conserved in bounce" prop_MomentumConserved,
    testProperty "energy conserved in bounce" prop_EnergyConserved
  ]

instance Arbitrary Position where
  arbitrary = createV <$> arbitrary <*> arbitrary

instance Arbitrary Velocity where
  arbitrary = createV <$> arbitrary <*> arbitrary

newtype BounceData = BounceData (Position, Velocity, Float)
  deriving (Show)

instance Arbitrary BounceData where
  arbitrary = fmap BounceData $ (,,) <$> arbitrary <*> arbitrary <*> massGenerator

massGenerator :: Gen Float
massGenerator = arbitrary `suchThat` (>0.0)

prop_MomentumConserved :: BounceData -> BounceData -> Bool
prop_MomentumConserved (BounceData x1@(_, v1, m1)) (BounceData x2@(_, v2, m2)) =
  let
    (v1', v2')      = bounce (800, 800) x1 x2
    momentumBefore  = scaleV m1 v1  `addV` scaleV m2 v2
    momentumAfter   = scaleV m1 v1' `addV` scaleV m2 v2'
    momentumDiff    = absV $ momentumAfter `subtractV` momentumBefore
  in momentumDiff `floatEqual` 0.0

prop_EnergyConserved :: BounceData -> BounceData -> Bool
prop_EnergyConserved (BounceData x1@(_, v1, m1)) (BounceData x2@(_, v2, m2)) =
  let
    (v1', v2')    = bounce (800, 800) x1 x2
    energyBefore  = m1 * absV v1 ** 2  + m2 * absV v2 ** 2
    energyAfter   = m1 * absV v1' ** 2 + m2 * absV v2' ** 2
  in energyBefore `floatEqual` energyAfter

floatEqual :: Float -> Float -> Bool
floatEqual 0 0 = True
floatEqual x 0 = abs x < 0.01
floatEqual 0 y = abs y < 0.01
floatEqual x y = abs (max x y / min x y) < 1.01
