package picc.connectors.primitives

import picc.connectors.Primitive
import picc.DSL._

/**
  * Created by jose on 27/02/16.
  */
class SDrain(a: String, b: String) extends Primitive(List(a,b)) {
  def getConstraint =  a <-> b
}
