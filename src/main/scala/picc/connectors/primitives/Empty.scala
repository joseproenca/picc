package picc.connectors.primitives

import picc.connectors.Primitive
import picc.connectors.constraints.Constraint

/**
  * Created by jose on 14/03/16.
  */
class Empty() extends Primitive(List()) {
  val getConstraint = Constraint()
}