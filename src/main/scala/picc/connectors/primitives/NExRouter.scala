package picc.connectors.primitives

import picc.connectors.Primitive
import picc.connectors.constraints._
import picc.DSL._

/**
  * Created by jose on 01/03/16.
  */
class NExRouter(src: String, snks: List[String]) extends Primitive(src::snks) {
  //  def v(x:String) = Var(flowVar(x, uid))


  private def orSnks = genSnkOr(snks)

  private def genSnkOr(lst: List[String]): Guard = lst match {
    case x :: Nil => x
    case x :: xs => x or genSnkOr(xs)
    case Nil => Neg(True)
  }

  private def genSnkAnd(lst: List[String]): Guard = lst match {
    case x :: Nil => x
    case x :: xs => x and genSnkAnd(xs)
    case Nil => True
  }

  private def genData(lst: List[String]): List[GuardedCommand] =
    for (snk <- snks) yield snk --> (snk := src) //VarAssgn(snk.data,src.data)

  val getConstraint = Constraint(
    src <-> orSnks,
//    orSnks --> src,
    if (snks.tail.isEmpty) True else Neg(genSnkAnd(snks))
  ) ++
    genData(snks)
}
