package picc.connectors.primitives

import picc.connectors.Primitive
import picc.connectors.constraints._
import picc.DSL._

/**
  * Created by jose on 27/02/16.
  */
class Filter(a: String, b: String,p: Predicate, positive:Boolean = true) extends Primitive(List(a,b)) {

  //  /**
  //   * Build guard (formula) from a Predicate
  //   * @param a source end
  //   * @param b sink end
  //   * @param uid unique channel id
  //   * @param p predicate
  //   */
  //  def this(a: String, b:String, uid: Int, p: Predicate) {
  //    this(a,b,uid,Pred(dataVar(a,uid),p))
  //  }

  //  /**
  //   * Build guard (formula) from a Predicate
  //   * @param a source end
  //   * @param b sink end
  //   * @param uid unique channel id
  //   * @param p predicate
  //   * @param positive if false consider the negation of the predicate
  //   */
  //  def this(a: String, b:String, uid: Int, p: Predicate, positive: Boolean) {
  //    this(a, b, uid, if (positive) Pred(dataVar(a,uid),p)
  //                    else      Neg(Pred(dataVar(a,uid),p)))
  //}

  protected def guard: Guard = if (positive) Pred(a,p)
  else      Neg(Pred(a,p))

  def getConstraint = Constraint(
    b --> a,
    b -->  (b := a),
    b --> guard,
    (a /\ guard) --> b
  )

}