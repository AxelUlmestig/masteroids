module Main where

import           Test.Framework                       (defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      (quickCheck)

import           Properties.Physics                   (properties)

main :: IO ()
main = defaultMain [
         testGroup "properties" $ fmap (uncurry testProperty) properties
       ]
