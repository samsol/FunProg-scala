package streams

import common._

/**
 * This component implements a parser to define terrains from a
 * graphical ASCII representation.
 * 
 * When mixing in that component, a level can be defined by
 * defining the field `level` in the following form:
 * 
 *   val level =
 *     """------
 *       |--ST--
 *       |--oo--
 *       |--oo--
 *       |------""".stripMargin
 * 
 * - The `-` character denotes parts which are outside the terrain
 * - `o` denotes fields which are part of the terrain
 * - `S` denotes the start position of the block (which is also considered
     inside the terrain)
 * - `T` denotes the final position of the block (which is also considered
     inside the terrain)
 * 
 * In this example, the first and last lines could be omitted, and
 * also the columns that consist of `-` characters only.
 */
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
  
  def isPostitionInTerrain(levelVector:Vector[Vector[Char]], pos:Pos):Boolean =  {
    pos match {
    	case Pos(x,y) =>x>=0&&y>=0 &&x<levelVector.length && y<levelVector(x).length && levelVector(x)(y) !='-' 
    }
    
  }
      
  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = isPostitionInTerrain(levelVector, _)

  /**
   * This function should return the position of character `c` in the
   * terrain described by `levelVector`. You can assume that the `c`
   * appears exactly once in the terrain.
   *
   * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
   * `Vector` class
   */
//  def findVector (vec : Vector[Char]): Boolean =   {
//    vec.toList.contains(c)
//  } 
  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    val vectorContainingChar = levelVector filter (vec => ((vec toList ).contains(c))) head
    val x = levelVector.indexOf(vectorContainingChar)
    val y = vectorContainingChar.indexOf(c)
    Pos(x,y)
  }

  private lazy val parseLevel: (Pos => Boolean, Pos, Pos) = {
    val vector: Vector[Vector[Char]] =
      Vector(level.split("\n").map(str => Vector(str: _*)): _*)

    val terrain: Pos => Boolean = terrainFunction(vector)
    val start = findChar('S', vector)
    val end = findChar('T', vector)

    (terrain, start, end)
  }

  lazy val terrain: Terrain = parseLevel._1
  lazy val startPos: Pos = parseLevel._2
  lazy val goal: Pos = parseLevel._3

}
