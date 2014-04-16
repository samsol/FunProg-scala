package streams

import common._

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   */
  def done(b: Block): Boolean =  { 
    (b.isStanding) && b.b1.x ==goal.x && b.b1.y ==goal.y
  }

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
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = 
      {
      val p = for { s <- b.neighbors if(s._1.isLegal) } yield {(s._1,s._2::history)} 
     
     p.toStream //append neighborsWithHistory(p.head._1, p.head._2) 
     //p.head #:: p.tail.head #::p.tail.tail.head #::p.tail.tail.tail.head #::neighborsWithHistory(p.head._1, p.head._2)
      // p #:: neighborsWithHistory(b, history)
      } 

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = 
                         
     {if (neighbors.isEmpty) Stream.Empty  
		  else if (explored contains neighbors.head._1) newNeighborsOnly(neighbors.tail, explored) 
		  	else neighbors.head#::newNeighborsOnly(neighbors.tail, explored + neighbors.head._1)
  
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
   * second time to avoid circles.
   * 
   * The resulting stream should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the stream.
   * 
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted stream.
   */
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    if (initial.isEmpty) Stream.Empty
    else {
       if (explored contains initial.head._1) from (initial.tail ,explored)
       //else   from (initial.tail,explored + initial.head._1) append 
       //				from(newNeighborsOnly(neighborsWithHistory(initial.head._1, initial.head._2), explored + initial.head._1),explored+initial.head._1)
       val newExplored = explored + initial.head._1
       //println("NEW EXPLORED :: "+newExplored)
       initial.head #:: from (initial.tail append(newNeighborsOnly(neighborsWithHistory(initial.head._1, initial.head._2), newExplored)),newExplored)
    }
  }

  /**
   * The stream of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = {
    val a = neighborsWithHistory(startBlock, List()) 
    from(a,Set())
    
  } //from(neighborsWithHistory(Block(startPos,startPos), List()))
    //neighborsWithHistory(Block(startPos,startPos), List())

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: Stream[(Block, List[Move])] = {
    for {
      p <- pathsFromStart 
      if done(p._1)
    }yield p 
  }

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] = pathsToGoal match {
  case Stream.Empty => List()
  case x#::xs => x._2 reverse 
}
}
