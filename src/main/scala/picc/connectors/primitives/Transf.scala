package picc.connectors.primitives

import picc.DSL._
import picc.connectors.Primitive
import picc.connectors.constraints.{Solution, OptSolution, Constraint, Function}

/**
  * Created by jose on 27/02/16.
  */
class Transf(a: String, b: String, f: Function) extends Primitive(List(a,b)) {

  val getConstraint = Constraint(
    a <-> b,
    (a --> (b := (f,a)))
  )
}

class TransfUndo (a: String, b: String, f: Function, undo: Function)
  extends Primitive(List(a,b)) {

  val newF = new Function { // wrapping function to guarantee its uniqueness (for a correct compensation)
    override def calculate(x:Any) = f.calculate(x)
    override def toString = f.toString
  }

  val transf = new Transf(a,b,newF)

  val getConstraint = transf.getConstraint

  override def update(s: OptSolution) {
    super.update(s)

    s match {
      case sol:Solution => s.getBuffer.rollback(newF,undo,sol getDataOn a)
      case _ => s.getBuffer.rollback(newF,undo,None)
    }
//    if (s.getBuffer.isDefined) {
//      //        println("trying to rollback")
//      s match {
//        case sol:Solution => sol.getBuffer.get.rollback(newF,undo,sol getDataOn a)
//        case _ => s.getBuffer.get.rollback(newF,undo,None)
//      }
//    }
//    else {
//      println("===== buffer not defined - required for compensation of transaction.")
//    }
  }
}