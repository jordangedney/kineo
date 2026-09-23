module Main (main) where

import ConfigSpec qualified
import CoreSpec qualified
import LayoutSpec qualified
import StripSpec qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "kineo"
      [StripSpec.tests, LayoutSpec.tests, CoreSpec.tests, ConfigSpec.tests]
