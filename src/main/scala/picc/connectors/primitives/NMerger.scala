package picc.connectors.primitives

import picc.connectors.Primitive
import picc.connectors.constraints._
import picc.DSL._

/**
  * Created by jose on 01/03/16.
  */
class NMerger (srcs: List[String], snk: String) extends Primitive(snk :: srcs) {

  private def orSrcs = genSrcOr(srcs)
  private def genSrcOr(lst:List[String]): Guard = lst match {
    case x::Nil => x
    case x::xs  => x or genSrcOr(xs)
    case Nil    => Neg(True)
  }
  private def genSrcAnd(lst:List[String]): Guard = lst match {
    case Nil    => True
    case x::Nil => Neg(True) // because of the negation before the or - no restrictions if there is 1 end.
    case x1::x2::Nil => x1 and x2
    case x::xs  => x and genSrcAnd(xs)
  }
  private def genData(lst:List[String]): List[GuardedCommand] =
    for (src <- srcs) yield src --> (snk := src) //VarAssgn(dataVar(snk,uid),dataVar(src,uid))

  val getConstraint = Constraint(
    snk <-> orSrcs,
    Neg(genSrcAnd(srcs))
  ) ++
    genData(srcs)

}
