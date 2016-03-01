package picc.connectors.primitives

import picc.connectors.Primitive
import picc.DSL._
import picc.connectors.constraints.{Solution, OptSolution, Constraint}

/**
  * Created by jose on 29/02/16.
  */
class Reader(x: String, var size: Int) extends Primitive(List(x)) {

  def getConstraint =
    if (size != 0) Constraint()
    else !x

  override def update(s: OptSolution) = s match {
    case sol:Solution => if (sol hasFlowOn x) {
      println("//////////////////")
      println("// Got data - "+x+": "+(sol getDataOn x))
      //      println("// new size: "+size)
      println("//////////////////")
      // println(s"sent data: ${data.head}")
      size -= 1
    }
    case _ => {}
  }

//  override def isProactive: Boolean = size > 0
//
//  // suggests which ends must have dataflow if "end" has also dataflow
//  def guessRequirements(end: String) = Set()
}