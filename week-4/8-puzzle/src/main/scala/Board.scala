

class Board(input: Array[Array[Int]]) {

  import Board._

  def dimension: Int = input.length

  val immutableInput = input.map(_.toList).toList

  val blocks: List[Block] =
    for {
      arrayWithIndex <- immutableInput.zipWithIndex
      ri = arrayWithIndex._2
      elementWithIndex <- arrayWithIndex._1.zipWithIndex
    } yield Block(ri, elementWithIndex._2, Some(elementWithIndex._1))

  // number of blocks out of place
  def hamming: Int = {
    def op(i: Int, b: Block): Int = {
      val x = (b.value.get - 1) / dimension
      val y = (b.value.get - 1) % dimension
      if(b.value.get == 0) i
      else if(b.row == x && b.column == y) i
      else i+1
    }
    blocks.foldLeft(0)(op)
  }

  // sum of Manhattan distances between blocks and goal
  def manhattan: Int = {
    def op(i: Int, b: Block): Int = {
      val x = (b.value.get - 1) / dimension
      val y = (b.value.get - 1) % dimension
      if(b.value.get == 0) i
      else {
        i + Math.abs(b.row - x) + Math.abs(b.column - y)
      }
    }
    blocks.foldLeft(0)(op)
  }

  def isGoal: Boolean =
    blocks.forall {b =>
      val x = (b.value.get - 1) / dimension
      val y = (b.value.get - 1) % dimension
      if(b.value.get == 0) true
      else b.row == x && b.column == y
    }


  // a board that is obtained by exchanging any pair of blocks
  def twin: Board = {
    val r = scala.util.Random
    val r1 = r.nextInt(dimension)
    val r2 = r.nextInt(dimension)
    val c1 = r.nextInt(dimension)
    val c2 = r.nextInt(dimension)
    val tmp = immutableInput.updated(r1, immutableInput(r1).updated(c1, input(r2)(c2)))
    val tmp1 = tmp.updated(r2, tmp(r2).updated(c2, input(r1)(c1)))
    new Board(tmp1.map(_.toArray).toArray)
  }

  def equals(that: Board): Boolean =
    this.blocks.sameElements(that.blocks)

  // all neighboring boards
  def neighbors: List[Board] = {
    val emptyBlock = blocks.filter(_.value.get == 0).head
    getNeighborBlocks(emptyBlock).map(swap(emptyBlock, _))
  }

  def getNeighborBlocks(b: Block): List[Block] = {
    var pairs: List[Block] = List.empty[Block]
    val row = b.row
    val column = b.column
    if(row+1 <= dimension-1) pairs = pairs  .+:(Block(row+1, column))
    if(row-1 >= 0) pairs = pairs.+:(Block(row-1, column))
    if(column+1 <= dimension-1) pairs = pairs.+:(Block(row, column+1))
    if(column-1 >= 0) pairs = pairs.+:(Block(row, column-1))
    pairs
  }

  def swap(empty: Block, block: Block): Board = {
    val erow = empty.row
    val ecol = empty.column
    val brow = block.row
    val bcol = block.column
    val tmp = immutableInput.updated(erow, immutableInput(erow).updated(ecol, input(brow)(bcol)))
    val tmp1 = tmp.updated(brow, tmp(brow).updated(bcol, 0))
    new Board(tmp1.map(_.toArray).toArray)
  }

  override def toString = {
    var s: StringBuilder = new StringBuilder()
    blocks.foreach{ b =>
      s.append(b.value.get + " ")
      if(b.column == dimension-1) s.append("\n")
    }
    s.toString()
  }

}

object Board {
  case class Block(row: Int, column: Int, value: Option[Int] = None)

  object ordering extends Ordering[Board] {
    def compare(a: Board, b: Board) = (a.hamming+a.manhattan) compare (b.hamming+b.manhattan)
  }
  implicit val ord = ordering.reverse
}