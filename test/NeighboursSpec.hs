module NeighboursSpec where
import           Neighbours
import           Test.Hspec

spec :: Spec
spec = describe "neighbours" $ do
  describe "leftCell" $ do
    it "the left cell of a left-most cell is always dead (top)" $ do
      leftCell 0 0 [[1,0,0],[0,0,0],[0,0,0]] `shouldBe` 0
    it "the left cell of a left-most cell is always dead (middle)" $ do
      leftCell 1 0 [[0,0,0],[1,0,0],[0,0,0]] `shouldBe` 0
    it "the left cell of a left-most cell is always dead (buttom)" $ do
      leftCell 2 0 [[0,0,0],[0,0,0],[1,0,0]] `shouldBe` 0

    it "has a living left neighbour" $ do
      leftCell 0 1 [[1,1,0],[0,0,0],[0,0,0]] `shouldBe` 1
    it "has a living left neighbour" $ do
      leftCell 1 1 [[0,0,0],[1,1,0],[0,0,0]] `shouldBe` 1
    it "has a living left neighbour" $ do
      leftCell 2 1 [[0,0,0],[0,0,0],[1,1,0]] `shouldBe` 1

    it "has a living left neighbour" $ do
      leftCell 0 2 [[0,1,1],[0,0,0],[0,0,0]] `shouldBe` 1
    it "has a living left neighbour" $ do
      leftCell 1 2 [[0,0,0],[0,1,1],[0,0,0]] `shouldBe` 1
    it "has a living left neighbour" $ do
      leftCell 2 2 [[0,0,0],[0,0,0],[0,1,1]] `shouldBe` 1

  describe "rightCell" $ do
    it "the right cell of a right-most cell is always dead (top)" $ do
      rightCell 0 2 [[0,0,1],[0,0,0],[0,0,0]] `shouldBe` 0
    it "the right cell of a right-most cell is always dead (middle)" $ do
      rightCell 1 2 [[0,0,0],[0,0,1],[0,0,0]] `shouldBe` 0
    it "the right cell of a right-most cell is always dead (buttom)" $ do
      rightCell 2 2 [[0,0,0],[0,0,0],[0,0,1]] `shouldBe` 0

    it "has a living right neighbour" $ do
      rightCell 0 1 [[0,1,1],[0,0,0],[0,0,0]] `shouldBe` 1
    it "has a living right neighbour" $ do
      rightCell 1 1 [[0,0,0],[0,1,1],[0,0,0]] `shouldBe` 1
    it "has a living right neighbour" $ do
      rightCell 2 1 [[0,0,0],[0,0,0],[0,1,1]] `shouldBe` 1

    it "has a living right neighbour" $ do
      rightCell 0 0 [[1,1,0],[0,0,0],[0,0,0]] `shouldBe` 1
    it "has a living right neighbour" $ do
      rightCell 1 0 [[0,0,0],[1,1,0],[0,0,0]] `shouldBe` 1
    it "has a living right neighbour" $ do
      rightCell 2 0 [[0,0,0],[0,0,0],[1,1,0]] `shouldBe` 1

  describe "upperCell" $ do
    it "the upper cell of a top-most cell is always dead" $ do
      upperCell 0 0 [[1,0,0],[0,0,0],[0,0,0]] `shouldBe` 0
    it "the upper cell of a top-most cell is always dead" $ do
      upperCell 0 1 [[0,1,0],[0,0,0],[0,0,0]] `shouldBe` 0
    it "the upper cell of a top-most cell is always dead" $ do
      upperCell 0 2 [[0,0,1],[0,0,0],[0,0,0]] `shouldBe` 0

    it "has a living upper neighbour" $ do
      upperCell 1 0 [[1,0,0],[1,0,0],[0,0,0]] `shouldBe` 1
    it "has a living upper neighbour" $ do
      upperCell 1 1 [[0,1,0],[0,1,0],[0,0,0]] `shouldBe` 1
    it "has a living upper neighbour" $ do
      upperCell 1 2 [[0,0,1],[0,0,1],[0,0,0]] `shouldBe` 1

    it "has a living upper neighbour" $ do
      upperCell 2 0 [[0,0,0],[1,0,0],[1,0,0]] `shouldBe` 1
    it "has a living upper neighbour" $ do
      upperCell 2 1 [[0,0,0],[0,1,0],[0,1,0]] `shouldBe` 1
    it "has a living upper neighbour" $ do
      upperCell 2 2 [[0,0,0],[0,0,1],[0,0,1]] `shouldBe` 1

  describe "lowerCell" $ do
    it "the lower cell of a bottom-most cell is always dead" $ do
      lowerCell 2 0 [[0,0,0],[0,0,0],[1,0,0]] `shouldBe` 0
    it "the lower cell of a bottom-most cell is always dead" $ do
      lowerCell 2 1 [[0,0,0],[0,0,0],[0,1,0]] `shouldBe` 0
    it "the lower cell of a bottom-most cell is always dead" $ do
      lowerCell 2 2 [[0,0,0],[0,0,0],[0,0,1]] `shouldBe` 0

    it "has a living lower neighbour" $ do
      lowerCell 0 0 [[1,0,0],[1,0,0],[0,0,0]] `shouldBe` 1
    it "has a living lower neighbour" $ do
      lowerCell 0 1 [[0,1,0],[0,1,0],[0,0,0]] `shouldBe` 1
    it "has a living lower neighbour" $ do
      lowerCell 0 2 [[0,0,1],[0,0,1],[0,0,0]] `shouldBe` 1

    it "has a living lower neighbour" $ do
      lowerCell 1 0 [[0,0,0],[1,0,0],[1,0,0]] `shouldBe` 1
    it "has a living lower neighbour" $ do
      lowerCell 1 1 [[0,0,0],[0,1,0],[0,1,0]] `shouldBe` 1
    it "has a living lower neighbour" $ do
      lowerCell 1 2 [[0,0,0],[0,0,1],[0,0,1]] `shouldBe` 1

  describe "upperLeftCell" $ do
    it "the upper-left cell of a left-most cell is always dead" $ do
      upperLeftCell 0 0 [[1,0,0],[0,0,0],[0,0,0]] `shouldBe` 0
    it "the upper-left cell of a left-most cell is always dead" $ do
      upperLeftCell 1 0 [[0,0,0],[1,0,0],[0,0,0]] `shouldBe` 0
    it "the upper-left cell of a left-most cell is always dead" $ do
      upperLeftCell 1 0 [[0,0,0],[0,0,0],[1,0,0]] `shouldBe` 0

    it "has a living neighbour" $ do
      upperLeftCell 1 1 [[1,0,0],[0,1,0],[0,0,0]] `shouldBe` 1
    it "has a living neighbour" $ do
      upperLeftCell 2 1 [[0,0,0],[1,0,0],[0,1,0]] `shouldBe` 1
    it "has a living neighbour" $ do
      upperLeftCell 1 2 [[0,1,0],[0,0,1],[0,0,0]] `shouldBe` 1
    it "has a living neighbour" $ do
      upperLeftCell 2 2 [[0,0,0],[0,1,0],[0,0,1]] `shouldBe` 1

  describe "lowerRightCell" $ do
    it "the lower-right cell of a right-most cell is always dead" $ do
      lowerRightCell 0 2 [[0,0,1],[0,0,0],[0,0,0]] `shouldBe` 0
    it "the lower-right cell of a right-most cell is always dead" $ do
      lowerRightCell 1 2 [[0,0,0],[0,0,1],[0,0,0]] `shouldBe` 0
    it "the lower-right cell of a right-most cell is always dead" $ do
      lowerRightCell 2 2 [[0,0,0],[0,0,0],[0,0,1]] `shouldBe` 0

    it "has a living neighbour" $ do
      lowerRightCell 0 0 [[1,0,0],[0,1,0],[0,0,0]] `shouldBe` 1
    it "has a living neighbour" $ do
      lowerRightCell 1 0 [[0,0,0],[1,0,0],[0,1,0]] `shouldBe` 1
    it "has a living neighbour" $ do
      lowerRightCell 0 1 [[0,1,0],[0,0,1],[0,0,0]] `shouldBe` 1
    it "has a living neighbour" $ do
      lowerRightCell 1 1 [[0,0,0],[0,1,0],[0,0,1]] `shouldBe` 1

  describe "upperRightCell" $ do
    it "the upper-right cell of a right-most cell is always dead" $ do
      upperRightCell 0 2 [[0,0,1],[0,0,0],[0,0,0]] `shouldBe` 0
    it "the upper-right cell of a right-most cell is always dead" $ do
      upperRightCell 1 2 [[0,0,0],[0,0,1],[0,0,0]] `shouldBe` 0
    it "the upper-right cell of a right-most cell is always dead" $ do
      upperRightCell 2 2 [[0,0,0],[0,0,0],[0,0,1]] `shouldBe` 0

    it "has a living neighbour" $ do
      upperRightCell 1 1 [[0,0,1],[0,1,0],[0,0,0]] `shouldBe` 1
    it "has a living neighbour" $ do
      upperRightCell 1 0 [[0,1,0],[1,0,0],[0,0,0]] `shouldBe` 1
    it "has a living neighbour" $ do
      upperRightCell 2 1 [[0,0,0],[0,0,1],[0,1,0]] `shouldBe` 1
    it "has a living neighbour" $ do
      upperRightCell 2 0 [[0,0,0],[0,1,0],[1,0,0]] `shouldBe` 1

  describe "lowerLeftCell" $ do
    it "the lower-left cell of a left-most cell is always dead" $ do
      lowerLeftCell 0 0 [[1,0,0],[0,0,0],[0,0,0]] `shouldBe` 0
    it "the lower-left cell of a left-most cell is always dead" $ do
      lowerLeftCell 1 0 [[0,0,0],[1,0,0],[0,0,0]] `shouldBe` 0
    it "the lower-left cell of a left-most cell is always dead" $ do
      lowerLeftCell 2 0 [[0,0,0],[0,0,0],[1,0,0]] `shouldBe` 0

    it "has a living neighbour" $ do
      lowerLeftCell 0 1 [[0,1,0],[1,0,0],[0,0,0]] `shouldBe` 1
    it "has a living neighbour" $ do
      lowerLeftCell 0 2 [[0,0,1],[0,1,0],[0,0,0]] `shouldBe` 1
    it "has a living neighbour" $ do
      lowerLeftCell 1 1 [[0,0,0],[0,1,0],[1,0,0]] `shouldBe` 1
    it "has a living neighbour" $ do
      lowerLeftCell 1 2 [[0,0,0],[0,0,1],[0,1,0]] `shouldBe` 1

  describe "neighboursOfCell" $ do
    it "a cell with two neighbours (left, right)" $ do
      neighboursOfCell 0 1 [[1,1,1],[0,0,0],[0,0,0]] `shouldBe` 2
    it "a cell with two neighbours (up, down)" $ do
      neighboursOfCell 1 1 [[0,1,0],[0,1,0],[0,1,0]] `shouldBe` 2

    it "a cell with eight neighbours (all directions)" $ do
      neighboursOfCell 1 1 [[1,1,1],[1,1,1],[1,1,1]] `shouldBe` 8

    it "left corner cell with three neighbours" $ do
      neighboursOfCell 0 0 [[1,1,0],[1,1,0],[0,0,0]] `shouldBe` 3
    it "left corner cell with three neighbours" $ do
      neighboursOfCell 2 0 [[0,0,0],[1,1,0],[1,1,0]] `shouldBe` 3
    it "right corner cell with three neighbours" $ do
      neighboursOfCell 0 2 [[0,1,1],[0,1,1],[0,0,0]] `shouldBe` 3
    it "right corner cell with three neighbours" $ do
      neighboursOfCell 2 2 [[0,0,0],[0,1,1],[0,1,1]] `shouldBe` 3

  describe "zipBoardWithIndices" $ do
    it "add indices" $ do
      zipBoardWithIndices 0 0 [[0,0,0],[0,0,0],[0,0,0]] `shouldBe` [[(0,0,0),(0,1,0),(0,2,0)],
                                                                [(1,0,0),(1,1,0),(1,2,0)],
                                                                [(2,0,0),(2,1,0),(2,2,0)]]
  describe "zipRowWithIndices" $ do
    it "add indices" $ do
      zipRowWithIndices 1 0 [0,0,0] `shouldBe` [(1,0,0), (1,1,0), (1,2,0)]

  describe "neighbours" $ do
    it "no living cells -> no neighbours" $ do
      neighbours [[0,0,0],[0,0,0],[0,0,0]] `shouldBe` [[0,0,0],[0,0,0],[0,0,0]]
    it "neighbours in a row" $ do
      neighbours [[1,1,1],[0,0,0],[0,0,0]] `shouldBe` [[1,2,1],[2,3,2],[0,0,0]]
    it "neighbours in a col" $ do
      neighbours [[0,1,0],[0,1,0],[0,1,0]] `shouldBe` [[2,1,2],[3,2,3],[2,1,2]]

  describe "nextGeneration" $ do
    it "no living cells -> no living cells" $ do
      nextGeneration [[0,0,0],[0,0,0],[0,0,0]] `shouldBe` [[0,0,0],[0,0,0],[0,0,0]]

  describe "notInBounds" $ do
    it "col too small" $ do
      notInBounds 0 (-1) [[0,0,0],[0,0,0],[0,0,0]] `shouldBe` True
    it "row too small" $ do
      notInBounds (-1) 0 [[0,0,0],[0,0,0],[0,0,0]] `shouldBe` True
    it "col and row too small" $ do
      notInBounds (-1) (-1) [[0,0,0],[0,0,0],[0,0,0]] `shouldBe` True

    it "col too big" $ do
      notInBounds 0 3 [[0,0,0],[0,0,0],[0,0,0]] `shouldBe` True
    it "row too big" $ do
      notInBounds 3 0 [[0,0,0],[0,0,0],[0,0,0]] `shouldBe` True
    it "col and row too big" $ do
      notInBounds 3 3 [[0,0,0],[0,0,0],[0,0,0]] `shouldBe` True
