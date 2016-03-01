package picc.connectors.constraints.choco

import collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 05/04/13.
 */
class DataMap {

//  val map: mutable.Map[Int,Any] = new mutable.HashMap[Int,Any]()
  val map = ArrayBuffer[Any]()
//  var seed = 0

  /**
   * Adds "x" and returns its index (from 2 - n+1)
   * @param x
   * @return
   */
  def add(x: Any): Int = {
    map += x
    map.size + 1
  }

  /**
   * Gets the element indexed by i (from 2 - n+1).
   * @param i must be > 1 (0 or 1 are reserved for booleans)
   * @return
   */
  def get(i: Int): Any = {
    if ((i > 1) && (i <= (map.length+1)))
      map(i-2)
    else
      null
  }

  def getAll: Iterable[Any] =
    map.toIterable

  override def toString(): String = map.mkString(",")
}
