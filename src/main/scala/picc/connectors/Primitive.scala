package picc.connectors

import picc.connectors.constraints.{OptSolution, Constraint}

/**
  * Abstract representation of a state machine with state given by
 * a collection of constraints `C` and an `update` method.
 * Connectors must be composable via a `++` method.
 *
 * Created by jose on 02/05/12.
 */

abstract class Primitive(val ends: List[String]) {

  /**
   * Collect the constraints and returns them, ready to be solved.
   */
  def getConstraint: Constraint

  /**
   * Given a (possible) solution updates the current state.
    *
    * @param s solution
   */
  def update(s: OptSolution) {} // default: do nothing


//  /**
//   * Updates the ID of the connector, e.g., to match the ID of its node or location.
//   * IDs are used to make variable names unique.
//   *
//   * @param newID the new ID of the connector.
//   */
//  def updateID(newId:Int)
//
//
//  /**
//   * Returns the current ID of the connector.
//   */
//  def getID: Int


  /**
   * Combine two primitives, resulting in a connector.
    * Note that the uid of the resulting connector is always given
    * by the left ID (or only existing ComplexConnector).
   *
   * @param other The other primitive to be composed
   * @return The composed connector
   */
  def ++(other: Primitive): Primitive =
    (this,other) match {
      case (x:Connector,_) => x +++ other
      case (_,x:Connector) => x +++ this
      case _ => new Connector(List(this,other),ends ++ other.ends)
    }

  /**
   * Collect constraints, solve them, and update connector.
   * Default solver for the constraints is used.
    *
    * @return Possible solution for the current step
   */
  def doStep: OptSolution = {
    val s = getConstraint.solve
    update(s)
    println("-- step done --\n"+s)
    s
  }

  /**
   * Keeps performing steps until no dataflow is possible.
   */
  def run() {
    if (doStep.isDefined) run()
  }


}

