module Tests

import Test.Golden.RunnerHelper

main : IO ()
main = goldenRunner
  [ "Documentation" `atDir` "docs"
  , "Interpolations" `atDir` "interpolations"
  ]
