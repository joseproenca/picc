package picc.connectors.primitives

import picc.connectors.Primitive
import picc.DSL._
import picc.connectors.constraints.{Neg, Constraint}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 27/02/16
 * Time: 14:13
 * To change this template use File | Settings | File Templates.
 */

class ExRouter(a: String, b: String, c: String) extends Primitive(List(a,b,c)) {
  val getConstraint = Constraint(
    a --> (b or c),
    (b or c) --> a,
    Neg(b and c),
    b --> (b := a),
    c --> (c := a)
  )
}