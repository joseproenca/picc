package picc.connectors.primitives

import picc.connectors.Primitive
import picc.connectors.constraints.Constraint
import picc.DSL._

/**
  * Created by jose on 01/03/16.
  */
class Lossy(a: String, b: String) extends Primitive(List(a,b)) {
  def getConstraint =  Constraint( b -> a, b := a )
}
