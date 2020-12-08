module Properties.Physics (properties) where

import           Test.QuickCheck (Arbitrary, Gen, arbitrary, suchThat)

import           Physics         (Position, Velocity, absV, addV, bounce,
                                  createV, scaleV, subtractV)

properties = [
    ("momentum conserved in bounce", prop_MomentumConserved)
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
    (v1', v2')      = bounce x1 x2
    momentum1       = scaleV m1 v1
    momentum1'      = scaleV m1 v1'
    momentum2       = scaleV m2 v2
    momentum2'      = scaleV m2 v2'
    momentumBefore  = momentum1 `addV` momentum2
    momentumAfter   = momentum1' `addV` momentum2'
    momentumDiff    = absV $ momentumAfter `subtractV` momentumBefore
  in momentumDiff `floatEqual` 0.0

floatEqual :: Float -> Float -> Bool
floatEqual x y = abs (x - y) < 0.001
