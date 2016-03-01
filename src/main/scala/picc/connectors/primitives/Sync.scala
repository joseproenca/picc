package picc.connectors.primitives

import picc.connectors.Primitive
import picc.DSL._
import picc.connectors.constraints.Constraint

/**
  * Created by jose on 27/02/16.
  */
class Sync(a: String, b: String) extends Primitive(List(a,b)) {
  def getConstraint =  Constraint( a <-> b, b := a )
}
