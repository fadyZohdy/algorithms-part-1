import scala.collection.mutable
import Board._

class Solver(board: Board) {

  private var movesMoved = 0
  private var flag = true
  private var enqueuedBoards = Set.empty[Board]

  /**
    * the game is solvable if number of inversions is even
    */
  def isSolvable: Boolean = {
    var inversions: BigInt = 0
    def go(blocks: List[Block]): List[Block] = {
      def merge(left: List[Block], right: List[Block]): List[Block] = (left, right) match {
        case (x :: xs, y :: ys) if x.value.get > y.value.get=> inversions += left.length; x :: merge(xs, right)
        case (x :: xs, y :: ys) => y :: merge(left, ys)
        case _ => if (left.isEmpty) right else left
      }
      val n = blocks.length / 2
      if (n == 0) blocks
      else {
        val (lowerHalf, upperHalf) = blocks splitAt n
        merge(go(lowerHalf), go(upperHalf))
      }
    }
    go(board.blocks.filterNot(_.value.get==0))
    inversions % 2 == 0
  }

  def moves: Int = movesMoved

  def solution: List[Board] = {
    var boards: List[Board] = List.empty[Board]
    val pq = new mutable.PriorityQueue[Board]
    pq.enqueue(board)
    enqueuedBoards = enqueuedBoards ++ Set(board)
    while(flag){
      val b = pq.dequeue()
//      println(b.toString)
      boards = boards :+ b
      if(b.isGoal) flag = false
      else {
        movesMoved += 1
        val neighbors = b.neighbors
        neighbors.foreach{neighbor =>
          if(!enqueuedBoards.exists(_.equals(neighbor))) {
            pq.enqueue(neighbor)
            enqueuedBoards = enqueuedBoards ++ Set(neighbor)
          }
        }
      }
      if(pq.isEmpty) flag=false
//      println("################################################")
    }
    boards
  }
}
