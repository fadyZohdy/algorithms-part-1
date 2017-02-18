
case class Percolation(n: Int) {
  import Percolation._

  private val dimension = 1 to n

  private var board: Board =
    (
      for {
        i <- dimension
        j <- dimension
      } yield Map((i, j) -> (false, (i, j)))
    ).flatten.toMap

  private var openSites = 0

  private var fullRoots: Map[Site,Boolean] = Map.empty

  // O(N)
  private def getRoot(s: Site): Site = {
    val root = board(s)._2
    if(s == root) s
    else getRoot(root)

  }

  private def connected(s1: Site, s2: Site): Boolean = getRoot(s1) == getRoot(s2)


  private def connect(s1: Site, s2: Site) = {
    val r1 = getRoot(s1)
    val r2 = getRoot(s2)
    if(fullRoots.isDefinedAt(r1) || fullRoots.isDefinedAt(r2) ||
       r1._1 == 1 || r2._1 == 1)
      fullRoots = fullRoots + (r2 -> true)
    if(!(r1 == r2)) board = board + (r1 -> (true, r2))
  }

  private def exists(s: Site): Boolean = {
    dimension.contains(s._1) && dimension.contains(s._2)
  }

  private def getOpenNeigbours(s: Site): List[Site] = {
    List(
      (s._1 + 1, s._2),
      (s._1 - 1, s._2),
      (s._1, s._2 + 1),
      (s._1, s._2 - 1)
    ) filter (site => exists(site) && isOpen(site))
  }

  def open(site: Site): Unit = {
    if(!isOpen(site) && exists(site)){
      board = board + (site -> (true, board(site)._2))
      openSites += 1
      getOpenNeigbours(site) foreach (s => connect(s, site))
    }
  }

  def isOpen(site: Site): Boolean = {
    board(site)._1
  }

  def isFull(site: Site): Boolean = fullRoots.isDefinedAt(getRoot(site))

  def numberOfOpenSites = openSites

  // O(N)
  def percolates: Boolean = {
    val bottomRowRoots = dimension map ((n, _)) map (getRoot)
    bottomRowRoots.exists(fullRoots.isDefinedAt)
  }

  def visualize = {
    for (i <- dimension) {
      for (j <- dimension){
        if(board((i,j))._1) print(s" ") else print("@")
      }
      print("\n")
    }
  }
}

object Percolation {
  type Site = (Int, Int)
  type Root = Site
  type Open = Boolean
  type Full = Boolean
  type Meta = (Open, Site)
  type Board = Map[Site, Meta]
}
