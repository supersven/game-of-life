module GameSpec where
import qualified Neighbours as Game
import           Test.Hspec

spec::Spec
spec = describe "next generation" $ do
  it "has no living cells, when there were none" $ do
    Game.nextGeneration [[0,0,0],[0,0,0],[0,0,0]] `shouldBe` [[0,0,0],
                                                              [0,0,0],
                                                              [0,0,0]]
  it "has no living cells, when there was only one" $ do
    Game.nextGeneration [[1,0,0],[0,0,0],[0,0,0]] `shouldBe` [[0,0,0],
                                                              [0,0,0],
                                                              [0,0,0]]
  it "has one living cell, when there were two neighbours in top row" $ do
    Game.nextGeneration [[1,1,0],[0,0,0],[0,0,0]] `shouldBe` [[0,0,0],
                                                              [1,1,0],
                                                              [0,0,0]]

  it "has one living cell, when there were three neigbours in a row (top)" $ do
    Game.nextGeneration [[1,1,1],[0,0,0],[0,0,0]] `shouldBe` [[0,1,0],
                                                              [1,1,1],
                                                              [0,0,0]]
  it "has one living cell, when there were three neigbours in a row (middle)" $ do
    Game.nextGeneration [[0,0,0],[1,1,1],[0,0,0]] `shouldBe` [[1,1,1],
                                                              [0,1,0],
                                                              [1,1,1]]
  it "has one living cell, when there were three neigbours in a row (buttom)" $ do
    Game.nextGeneration [[0,0,0],[0,0,0],[1,1,1]] `shouldBe` [[0,0,0],
                                                              [1,1,1],
                                                              [0,1,0]]

  it "has one living cell, when there were three neigbours in a column (left)" $ do
    Game.nextGeneration [[1,0,0],[1,0,0],[1,0,0]] `shouldBe` [[0,1,0],
                                                              [1,1,0],
                                                              [0,1,0]]
  it "has one living cell, when there were three neigbours in a column (middle)" $ do
    Game.nextGeneration [[0,1,0],[0,1,0],[0,1,0]] `shouldBe` [[1,0,1],
                                                              [1,1,1],
                                                              [1,0,1]]
  it "has one living cell, when there were three neigbours in a column (buttom)" $ do
    Game.nextGeneration [[0,0,1],[0,0,1],[0,0,1]] `shouldBe` [[0,1,0],
                                                              [0,1,1],
                                                              [0,1,0]]
