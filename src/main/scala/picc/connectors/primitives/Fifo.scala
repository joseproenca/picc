package picc.connectors.primitives

import picc.connectors.Primitive
import picc.connectors.constraints.{OptSolution, Constraint}
import picc.connectors.constraints.Solution
import picc.DSL._

/**
  * Created by jose on 27/02/16.
  */
class Fifo(a: String, b: String, var data: Option[Any]) extends Primitive(List(a,b)) {

  //  def this(a: String, b: String, dt: Option[Int], uid: Int) = this(a,b, dt.map(Int.box(_)),uid)
  def this(a: String, b: String) = this(a,b, scala.None:Option[Any])

  private def emptyFifo = Constraint(!b)

  private def fullFifo: Constraint =
    Constraint(
      !a,
      b --> (b :== data.get)
    )

  def getConstraint = if (data.isDefined) fullFifo else emptyFifo

  override def update(s: OptSolution) = s match  {
    case sol:Solution =>
      if (sol hasFlowOn a) {
        data = Some(sol(a))
        // println("FIFO: FLOW IN!")
      }
      else if (sol hasFlowOn b) {
        data = None
        // println("FIFO: FLOW OUT!")
      }
    case _ => {}
  }
}
