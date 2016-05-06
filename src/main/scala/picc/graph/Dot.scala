package picc.graph

import picc.connectors.constraints._
import picc.connectors.{Primitive, Connector}

/**
  * Created by jose on 04/05/16.
  */
object Dot {

  /**
    * Produces a dot-graph approximation based on heuristics that determine the direction of dataflow.
    * @param p primitive to be drawn
    * @return the dot representation (string)
    */
  def apply(p:Primitive): String = {
    s"digraph G {\n${getEdges(p)}}"
  }

  def getEdges(p:Primitive): String = p match {
    case c:Connector => getCEdges(c)
    case c:Primitive => getPEdges(c)
  }

  private def getCEdges(c: Connector): String = {
    val res = new StringBuilder
    for (p <- c.sub)
      res append getEdges(p)
    res.toString()
  }

  private def getPEdges(p:Primitive): String = {
    val res = new StringBuilder
    val (ins,outs)  = inferDir(p.getConstraint)
    var ios  = p.ends.toSet -- ins -- outs
    for (i <- ins; o <- outs)
      res append s"  $i -> $o [label=${namePrim(p)}];\n"
    for (i <- ins; o <- ios)
      res append s"  $i -> $o [label=${namePrim(p)}];\n"
    for (i <- ios; o <- outs)
      res append s"  $i -> $o [label=${namePrim(p)}];\n"
    for (i <- ios; o <- ios; if p.ends.indexOf(i) < p.ends.indexOf(o))
      res append s"  $i -> $o [dir=none,label=${namePrim(p)}];\n" //dir=none/both
    for (i <- ins; o <- ins; if p.ends.indexOf(i) < p.ends.indexOf(o))
      res append s"  $i -> $o [dir=none,label=${namePrim(p)}];\n" //dir=none/both
    for (i <- outs; o <- outs; if p.ends.indexOf(i) < p.ends.indexOf(o))
      res append s"  $i -> $o [dir=none,label=${namePrim(p)}];\n" //dir=none/both
    p.ends match {
      case (end::Nil) =>
        if (ins  contains end) res append s"  $end -> ${namePrim(p)};" else
        if (outs contains end) res append s"  ${namePrim(p)} -> $end;" else
                               res append s"  $end -> ${namePrim(p)} [dir=none];"
      case _ =>
    }
    res.toString()
  }

  private def namePrim(p:Primitive) = p.getClass.getSimpleName

  private def inferDir(c:Constraint): (Set[String],Set[String]) = {
    var ins = Set[String]()
    var outs = Set[String]()
    for (com <- c.commands) {
      val (in,out) = inferDir(com.st)
      ins  ++= in
      outs ++= out
    }
    (ins,outs)
  }

  private def inferDir(s:Statement): (Set[String],Set[String]) = s match {
    case IntAssgn(v, d) => (Set(),Set(v))
    case VarAssgn(v1, v2) => (Set(v2),Set(v1))
    case FunAssgn(v1, v2, f) => (Set(v2),Set(v1))
    case NFunAssgn(v1, vs, f) => (vs.toSet,Set(v1))
    case DataAssgn(v, d) => (Set(),Set(v))
    case Seq(Nil) => (Set(),Set())
    case Seq(s2::sts) =>
      val (i1,o1) = inferDir(s2)
      val (i2,o2) = inferDir(Seq(sts))
      (i1++i2,o1++o2)
    case _: Guard => (Set(),Set())
  }

}
