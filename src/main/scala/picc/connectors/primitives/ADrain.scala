package picc.connectors.primitives

import picc.connectors.Primitive
import picc.DSL._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/07/12
 * Time: 11:23
 * To change this template use File | Settings | File Templates.
 */

class ADrain (a: String, b: String) extends Primitive(List(a,b)) {
  def getConstraint = !(a and b)
}
