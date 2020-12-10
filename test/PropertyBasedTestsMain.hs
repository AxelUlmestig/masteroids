module Main where

import           Test.Framework     (defaultMain)

import           Properties.Physics (physicsProperties)

main :: IO ()
main = defaultMain [
         physicsProperties
       ]
