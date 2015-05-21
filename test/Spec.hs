import           Test.Hspec

import qualified GameSpec
import qualified NeighboursSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Integration Tests" GameSpec.spec
  describe "Neighbour Tests" NeighboursSpec.spec
