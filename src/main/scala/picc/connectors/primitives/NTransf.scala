package picc.connectors.primitives

import picc.connectors.Primitive
import picc.connectors.constraints.{GuardedCommand, Constraint, Function}
import picc.DSL._

/**
  * Created by jose on 29/02/16.
  */
class NTransf (ans: List[String], b: String, f: Function) extends Primitive(ans ++ List(b)) {
//  private val as = ans.map(mkVar(_))
  //  private val b:Var = "b" // mkVar(bn, uid) // Var(flowVar(bn,uid)) // implicit conversion

  val getConstraint = Constraint(
    for (a<-ans) yield (a <-> b):GuardedCommand
  ) ++
    (b --> (b := (f,ans)))

}
