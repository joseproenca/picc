package picc.connectors

import picc.connectors.constraints.{OptSolution, Constraint}
import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/11/12
 * Time: 16:31
 * To change this template use File | Settings | File Templates.
 */
class Connector(val sub: List[Primitive], ends: List[String])
    extends Primitive(ends) {

  /**
   * Collect the constraints and returns them, ready to be solved.
   * Add sync rules: assume shared variables for synchronisation,
   *   and connect CC3-related variables (assuming their original ids - good!)
   */
  def getConstraint = {
    var res = Constraint()
    for (c <- sub) res ++= c.getConstraint
    res
  }

  override def update(s: OptSolution) {
    for (c <- sub) c.update(s)
  }

  def +++(other: Primitive): Connector = other match {
    // Note: could drop repeated names, if needed.
    case c: Connector => new Connector(sub ++ c.sub,ends ++ c.ends)
    case _ => new Connector(other :: sub, ends ++ other.ends)
  }
}
