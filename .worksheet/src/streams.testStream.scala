package streams

object testStream {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(80); 
  println("Welcome to the Scala worksheet")

  trait GameDef {

    /**
     * The case class `Pos` encodes positions in the terrain.
     *
     * IMPORTANT NOTE
     *  - The `row` coordinate denotes the position on the vertical axis
     *  - The `col` coordinate is used for the horizontal axis
     *  - The coordinates increase when moving down and right
     *
     * Illustration:
     *
     *     0 1 2 3   <- col axis
     *   0 o o o o
     *   1 o o o o
     *   2 o # o o    # is at position Pos(2, 1)
     *   3 o o o o
     *
     *   ^
     *   |
     *
     *   row axis
     */
    case class Pos(row: Int, col: Int) {
      /** The position obtained by changing the `row` coordinate by `d` */
      def deltaRow(d: Int): Pos = copy(row = row + d)

      /** The position obtained by changing the `col` coordinate by `d` */
      def deltaCol(d: Int): Pos = copy(col = col + d)
    }

    /**
     * The position where the block is located initially.
     *
     * This value is left abstract, it will be defined in concrete
     * instances of the game.
     */
    val startPos: Pos

    /**
     * The target position where the block has to go.
     * This value is left abstract.
     */
    val goal: Pos

    /**
     * The terrain is represented as a function from positions to
     * booleans. The function returns `true` for every position that
     * is inside the terrain.
     *
     * As explained in the documentation of class `Pos`, the `row` axis
     * is the vertical one and increases from top to bottom.
     */
    type Terrain = Pos => Boolean

    /**
     * The terrain of this game. This value is left abstract.
     */
    val terrain: Terrain

    /**
     * In Bloxorz, we can move left, right, Up or down.
     * These moves are encoded as case objects.
     */
    sealed abstract class Move
    case object Left extends Move
    case object Right extends Move
    case object Up extends Move
    case object Down extends Move

    /**
     * This function returns the block at the start position of
     * the game.
     */
    def startBlock: Block = Block(startPos, startPos)

    /**
     * A block is represented by the position of the two cubes that
     * it consists of. We make sure that `b1` is lexicographically
     * smaller than `b2`.
     */
    case class Block(b1: Pos, b2: Pos) {

      // checks the requirement mentioned above
      require(b1.row <= b2.row && b1.col <= b2.col, "Invalid block position: b1=" + b1 + ", b2=" + b2)

      /**
       * Returns a block where the `row` coordinates of `b1` and `b2` are
       * changed by `d1` and `d2`, respectively.
       */
      def deltaRow(d1: Int, d2: Int) = Block(b1.deltaRow(d1), b2.deltaRow(d2))

      /**
       * Returns a block where the `col` coordinates of `b1` and `b2` are
       * changed by `d1` and `d2`, respectively.
       */
      def deltaCol(d1: Int, d2: Int) = Block(b1.deltaCol(d1), b2.deltaCol(d2))

      /** The block obtained by moving left */
      def left = if (isStanding) deltaCol(-2, -1)
      else if (b1.row == b2.row) deltaCol(-1, -2)
      else deltaCol(-1, -1)

      /** The block obtained by moving right */
      def right = if (isStanding) deltaCol(1, 2)
      else if (b1.row == b2.row) deltaCol(2, 1)
      else deltaCol(1, 1)

      /** The block obtained by moving up */
      def up = if (isStanding) deltaRow(-2, -1)
      else if (b1.row == b2.row) deltaRow(-1, -1)
      else deltaRow(-1, -2)

      /** The block obtained by moving down */
      def down = if (isStanding) deltaRow(1, 2)
      else if (b1.row == b2.row) deltaRow(1, 1)
      else deltaRow(2, 1)

      /**
       * Returns the list of blocks that can be obtained by moving
       * the current block, together with the corresponding move.
       */
      def neighbors: List[(Block, Move)] = {
        val blocks = List(left, right, up, down)
        val moves = List(Left, Right, Up, Down)
        blocks zip moves
      }

      /**
       * Returns the list of positions reachable from the current block
       * which are inside the terrain.
       */
      def legalNeighbors: List[(Block, Move)] = {
        for {
          (block, move) <- neighbors
          if block.isLegal
        } yield (block, move)
      }

      /**
       * Returns `true` if the block is standing.
       */
      def isStanding: Boolean = {
        b1.row == b2.row && b1.col == b2.col
      }

      /**
       * Returns `true` if the block is entirely inside the terrain.
       */
      def isLegal: Boolean = {
        terrain(b1) && terrain(b2)
      }
    }
  }

  trait StringParserTerrain extends GameDef {

    /**
     * A ASCII representation of the terrain. This field should remain
     * abstract here.
     */
    val level: String

    /**
     * This method returns terrain function that represents the terrain
     * in `levelVector`. The vector contains parsed version of the `level`
     * string. For example, the following level
     *
     *   val level =
     *     """ST
     *       |oo
     *       |oo""".stripMargin
     *
     * is represented as
     *
     *   Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
     *
     * The resulting function should return `true` if the position `pos` is
     * a valid position (not a '-' character) inside the terrain described
     * by `levelVector`.
     */
    def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = (pos: Pos) => {
      levelVector(pos.row)(pos.col) != '-'
    }

    /**
     * This function should return the position of character `c` in the
     * terrain described by `levelVector`. You can assume that the `c`
     * appears exactly once in the terrain.
     *
     * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
     * `Vector` class
     */
    def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
      val pos = for {
        row <- levelVector
        char <- row
        if char == c
      } yield Pos(levelVector.indexOf(row), row.indexOf(char))
      pos(0)
    }

    private lazy val vector: Vector[Vector[Char]] =
      Vector(level.split("\n").map(str => Vector(str: _*)): _*)

    lazy val terrain: Terrain = terrainFunction(vector)
    lazy val startPos: Pos = findChar('S', vector)
    lazy val goal: Pos = findChar('T', vector)
  }

  trait Solver extends GameDef {

    /**
     * Returns `true` if the block `b` is at the final position
     */
    def done(b: Block): Boolean = { b.b1 == goal && b.b2 == goal }

    /**
     * This function takes two arguments: the current block `b` and
     * a list of moves `history` that was required to reach the
     * position of `b`.
     *
     * The `head` element of the `history` list is the latest move
     * that was executed, i.e. the last move that was performed for
     * the block to end up at position `b`.
     *
     * The function returns a stream of pairs: the first element of
     * the each pair is a neighboring block, and the second element
     * is the augmented history of moves required to reach this block.
     *
     * It should only return valid neighbors, i.e. block positions
     * that are inside the terrain.
     */
    def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
      if (history.isEmpty) Stream.empty
      else {
        val neighbors = for {
          (neighbor, move) <- b.legalNeighbors
        } yield (neighbor, move :: history)
        neighbors.toStream
      }
    }

    /**
     * This function returns the list of neighbors without the block
     * positions that have already been explored. We will use it to
     * make sure that we don't explore circular paths.
     */
    def newNeighborsOnly(
      neighbors: Stream[(Block, List[Move])],
      explored:  Set[Block]): Stream[(Block, List[Move])] = {
      val newNeighbors = for {
        (neighbor, moves) <- neighbors.toList
        if !explored.contains(neighbor)
      } yield (neighbor, moves)
      newNeighbors.toStream
    }

    /**
     * The function `from` returns the stream of all possible paths
     * that can be followed, starting at the `head` of the `initial`
     * stream.
     *
     * The blocks in the stream `initial` are sorted by ascending path
     * length: the block positions with the shortest paths (length of
     * move list) are at the head of the stream.
     *
     * The parameter `explored` is a set of block positions that have
     * been visited before, on the path to any of the blocks in the
     * stream `initial`. When search reaches a block that has already
     * been explored before, that position should not be included a
     * second time to avoid cycles.
     *
     * The resulting stream should be sorted by ascending path length,
     * i.e. the block positions that can be reached with the fewest
     * amount of moves should appear first in the stream.
     *
     * Note: the solution should not look at or compare the lengths
     * of different paths - the implementation should naturally
     * construct the correctly sorted stream.
     */
    def from(
      initial:  Stream[(Block, List[Move])],
      explored: Set[Block]): Stream[(Block, List[Move])] = {
      if (initial.isEmpty) Stream.empty
      else {
        val next = for {
          (block, history) <- initial
          neighbors = neighborsWithHistory(block, history)
          newNeighbor <- newNeighborsOnly(neighbors, explored)
        } yield newNeighbor
        initial ++ from(next, explored ++ (next map (_._1)).toSet) // given path set plus next generation "more"
      }
    }

    /**
     * The stream of all paths that begin at the starting block.
     */
    lazy val pathsFromStart: Stream[(Block, List[Move])] = from(Stream((startBlock, Nil)), Set(startBlock))

    /**
     * Returns a stream of all possible pairs of the goal block along
     * with the history how it was reached.
     */
    lazy val pathsToGoal: Stream[(Block, List[Move])] = {
      // All solution paths ordered by length
      for {
        (block, path) <- pathsFromStart
        if done(block)
      } yield (block, path) //why end up being in a stream ?
    }

    /**
     * The (or one of the) shortest sequence(s) of moves to reach the
     * goal. If the goal cannot be reached, the empty list is returned.
     *
     * Note: the `head` element of the returned list should represent
     * the first move that the player should perform from the starting
     * position.
     */
    lazy val solution: List[Move] = pathsToGoal.head._2
  }

  abstract class Level extends Solver with StringParserTerrain

  object Level0 extends Level {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  };$skip(10765); val res$0 = 
  Level0.solution;System.out.println("""res0: List[streams.testStream.Level0.Move] = """ + $show(res$0))}

}
