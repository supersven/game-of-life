import           Test.Hspec

import qualified GameSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Integration Tests" GameSpec.spec
