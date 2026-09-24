module Main (main) where

import CheatsheetSpec qualified
import ConfigSpec qualified
import CoreSpec qualified
import LayoutSpec qualified
import MotionSpec qualified
import StripSpec qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "kineo"
      [StripSpec.tests, LayoutSpec.tests, MotionSpec.tests, CoreSpec.tests, ConfigSpec.tests, CheatsheetSpec.tests]
