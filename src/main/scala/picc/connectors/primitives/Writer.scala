package picc.connectors.primitives

import picc.connectors.Primitive
import picc.DSL._
import picc.connectors.constraints.{Solution, OptSolution}


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/07/12
 * Time: 11:15
 * To change this template use File | Settings | File Templates.
 */

class Writer (val x: String, var data: List[Any]) extends Primitive(List(x)) {

//  def this(x: String, uid: Int, dt: List[Int]) = this(x, uid, dt.map(Int.box(_)))
  def this(x: String) = this(x, Nil: List[Any])

  def getConstraint = {
    if (data.nonEmpty)
        x --> (x :== data.head)
    else !x
  }

  override def update(s: OptSolution) {
    s match {
      case sol:Solution => if (sol hasFlowOn x) {
        // println(s"sent data: ${data.head}")
        data = data.tail
      }
      case _ => {}
    }
  }

//  override def isProactive: Boolean = !data.isEmpty

  // suggests which ends must have dataflow if "end" has also dataflow
//  def guessRequirements(end: String) = Set()

}

