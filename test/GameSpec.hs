module GameSpec where
import qualified GameOfLife as Game
import           Test.Hspec

spec::Spec
spec = describe "next generation" $ do
  it "has no living cells, when there were none in the previous generation" $ do
    Game.nextGeneration [[0,0,0],[0,0,0],[0,0,0]] `shouldBe` [[0,0,0],[0,0,0],[0,0,0]]
  it "has no living cells, when there was only one in the previous generation" $ do
    Game.nextGeneration [[1,0,0],[0,0,0],[0,0,0]] `shouldBe` [[0,0,0],[0,0,0],[0,0,0]]
  it "has no living cells, when there were only two neighbours in the previous generation" $ do
    Game.nextGeneration [[1,1,0],[0,0,0],[0,0,0]] `shouldBe` [[0,0,0],[0,0,0],[0,0,0]]

  it "has one living cell, when there were three neigbours in the previous generation" $ do
    Game.nextGeneration [[1,1,1],[0,0,0],[0,0,0]] `shouldBe` [[0,1,0],[0,0,0],[0,0,0]]
