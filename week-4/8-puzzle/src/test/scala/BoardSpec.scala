import Board.Block
import org.scalatest.FlatSpec

class BoardSpec extends FlatSpec {

  val validBlocks: Array[Array[Int]] = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 0))
  val validBoard = new Board(validBlocks)

  val invalidBlocks: Array[Array[Int]] = Array(Array(8, 1 ,3), Array(4, 0, 2), Array(7, 6, 5))
  val invalidBoard: Board = new Board(invalidBlocks)

  val invalidBlocks2: Array[Array[Int]] = Array(Array(8, 1 ,3), Array(4, 0, 2), Array(7, 6, 5))
  val invalidBoard2: Board = new Board(invalidBlocks)

  "hamming function" should "return 5" in {
    assert(invalidBoard.hamming == 5)
  }

  "hamming function" should "return 4" in {
    val aa: Array[Array[Int]] = Array(Array(1, 2, 3), Array(0, 4, 5), Array(7, 8, 6))
    val ba = new Board(aa)
    assert(ba.hamming == 3)
  }

  "hamming function 2" should "return 5" in {
    val aa: Array[Array[Int]] = Array(Array(4, 1, 3), Array(0, 2, 5), Array(7, 8, 6))
    val ba = new Board(aa)
    assert(ba.hamming == 5)
  }

  "manhattan function" should "return 10" in {
    assert(invalidBoard.manhattan == 10)
  }

  "manhattan function" should "return 3" in {
    val aa: Array[Array[Int]] = Array(Array(1, 2, 3), Array(0, 4, 5), Array(7, 8, 6))
    val ba = new Board(aa)
    assert(ba.manhattan == 3)
  }

  "isGoal function" should "return false" in {
    assert(!invalidBoard.isGoal)
  }

  "isGoal function" should "return true" in {
    assert(validBoard.isGoal)
  }

  "equals function" should "return true" in {
    assert(invalidBoard.equals(invalidBoard2))
  }

  "getNeigborBlocks function" should "return 4 blocks" in {
    assert(invalidBoard.getNeighborBlocks(Block(1,1, None)).length == 4)
  }

  "getNeigborBlocks function" should "return 2 blocks" in {
    assert(invalidBoard.getNeighborBlocks(Block(2,2, None)).length == 2)
  }

  "getNeigborBlocks function" should "return 3 blocks" in {
    assert(invalidBoard.getNeighborBlocks(Block(1,2, None)).length == 3)
  }

  "swap function" should "return swapped board" in {
    assert(validBoard.isGoal)
    val board = validBoard.swap(Block(2,2,None), Block(1,2,None))
    assert(!board.isGoal)
    assert(board.blocks.filter(_.value.get == 0).head.row == 1)
  }

  "neigbors function" should "return 4 boards" in {
    assert(invalidBoard.neighbors.length == 4)
  }
}