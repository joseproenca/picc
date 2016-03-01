package picc.connectors.constraints

import picc.connectors.constraints.choco.Solver

import collection.mutable.{Set => MutSet, Map => MutMap, ListBuffer}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 18:14
 * To change this template use File | Settings | File Templates.
 */

abstract class Constraint {

  private var log = System.out
//  val log = new java.io.PrintStream(new java.io.FileOutputStream("/dev/null"))

  val commands: Set[GuardedCommand] //= Set[GuardedCom]()

  def solve: OptSolution = Solver.solve(this,None) //new NoSolution(new Buffer)

  override def toString = commands.mkString("\n")

//  def withID(uid:Int) : Constraint = {
//	  val thisComms = commands
// 	  new Constraint() {
//		  override val commands = thisComms.map(GuardedCom.addID(_,uid))
//	  }
//  }


//  def visitVars(f:String => String):

  /**
   * Combines the *boolean* free variables of all guarded commands efficiently
    *
    * @return boolean free variables of all guarded commands
   */
  def fv(l:ListBuffer[String]) { for (c<-commands) c.fv(l) }
  //commands.map(_.bfv).foldRight[Set[String]](Set())(_++_)


//  /**
//   * Add SOME-FLOW and CC3 constraints.
//   * CC3 not yet in use.
//   * Used before using Choco (predicate abstraction + lazy & Choco as SMT).
//   * Other approaches (Z3 and SAT4J) use optimised version.
//   */
//  def close(): Constraint = {
//    var comms = Set[GuardedCom]()
////    println("## closing...")
//    if (!closed) {
////      println("## not closed yet...")
//      val bvars = new ListBuffer[String]()
//      bfv(bvars)
////      println("## b vars: "+bvars.size)
//      if (!bvars.isEmpty) {
//        val fst = bvars.head
//        var flowConstraint: Guard = Var(fst)
//        for (v <- bvars.tail)
//          flowConstraint = flowConstraint \/  Var(v)
////        println("closing - adding "+flowConstraint)
//        comms += flowConstraint
//      }
//      val thisComms = commands
//      new Constraint {
//        override val commands = thisComms ++ comms
//        override val closed = true
//      }
//    }
//    else this
//  }




  /**
   * Solve solutions using Choco with external predicates/functions, using numbers as data
   * that serve as ID in a dynamic table of data.
    *
    * @return Solution for the data constraints.
   */
//  def solveChocoDyn: OptionSol[DynSolution] = solveChocoDyn(None)


  /**
   * Solve solutions using Choco and external predicates/functions, based on a dynamic map from ints to data values.
    *
    * @return Solution for the data constraints.
   */
//  def solveChocoDyn(tried:Option[NoneSol]): OptionSol[DynSolution] = {
//    val closed = close()
//    ChocoDyn.solve(closed,tried)
//  }


  ///////////////////////

  def ++(other: Constraint): Constraint = {
    val thiscommands = commands
    new Constraint(){
      override val commands = thiscommands ++ other.commands
    }
  }
  def ++(others: Iterable[GuardedCommand]): Constraint = {
    val thiscommands = commands
    new Constraint(){
      override val commands = thiscommands ++ others
    }
  }

  def ++(other: GuardedCommand): Constraint = {
    val thiscommands = commands
    new Constraint(){
      override val commands = thiscommands + other
    }
  }
}


object Constraint {
  def apply(gs: Iterable[GuardedCommand]): Constraint = {
    new Constraint() {
    	override val commands = gs.toSet
  	}
  }
  def apply(gs: GuardedCommand*): Constraint = apply(gs.toSet)
  def apply(g: GuardedCommand): Constraint = apply(Set(g))
  def apply(): Constraint = apply(List[GuardedCommand]())
}

case class GuardedCommand(g:Guard, st: Statement) {
  def fv(l: ListBuffer[String]) = {
    g.fv(l);
    st.fv(l)
  }
}

///// STATMENTENTS ////
abstract sealed class Statement {
  // syntactic sugar: Seq(...) (and)
  def /\(s: Statement) = and(s)
  def and(s: Statement) = this match {
    case Seq(s1) => s match {
      case Seq(s2) => Seq(s1 ::: s2)
      case y => Seq(s1 ::: List(y))
    }
    case x => s match {
      case Seq(s2) => Seq(x :: s2)
      case y => Seq(List(this, s))
    }
  }

  // free vars (efficient version)
  def fv(l: ListBuffer[String]): Unit = this match {
    case g: Guard => g.fv(l)
    case Seq(Nil) =>
    case Seq(s :: ss) => {
      s.fv(l); Seq(ss).fv(l)
    }
    case _ =>
  }
}
/// concrete STATEMENTS
case class IntAssgn(v: String, d: Int) extends Statement
case class VarAssgn(v1: String, v2: String) extends Statement
case class FunAssgn(v1:String, v2:String, f: Function) extends Statement
case class NFunAssgn(v1:String,vs:List[String], f: Function) extends Statement
case class DataAssgn(v: String, d: Any) extends Statement
case class Seq(sts: List[Statement]) extends Statement


///// GUARDS ////
abstract sealed class Guard extends Statement {
  def and(e: Guard) = And(this, e)
  def /\(e: Guard) = And(this, e)
  def or(e: Guard) = Or(this, e)
  def \/(e: Guard) = Or(this, e)
  def ->(e: Guard) = Impl(this, e)
  def -->(e: Statement) = GuardedCommand(this, e)
  def <->(e: Guard) = Equiv(this, e)
  def unary_! = Neg(this)

  override def fv(l: ListBuffer[String]): Unit = this match {
    case Var(name) => if (!l.contains(name)) l += name
    case And(g1, g2) => {
      g1.fv(l); g2.fv(l)
    }
    case Or(g1, g2) => {
      g1.fv(l); g2.fv(l)
    }
    case Neg(g) => g.fv(l)
    case Impl(g1, g2) => {
      g1.fv(l); g2.fv(l)
    }
    case Equiv(g1, g2) => {
      g1.fv(l); g2.fv(l)
    }
    case _ => {}
  }
}

case class Var(name: String) extends Guard {
  /** Assignment of data variables. */
  def :=(v:Var): Statement = VarAssgn(name,v.name) //VarAssgn(toDataVar(name),toDataVar(v.name))
  /** Assignment of data values. */
  def :== (d:Any): Statement = DataAssgn(name,d) //DataAssgn(toDataVar(name),d)
//  def :=(d: Any): Statement = d match {
//    case (v:Var) => VarAssgn(name,v.name)
//    case _ => DataAssgn(name,d)
//  }
  /** Application of a function to a var, and assignment of the result. */
  def :=(f:Function,dv:Var): Statement =
    FunAssgn(name,dv.name,f) //FunAssgn(toDataVar(name),toDataVar(v.name),f)
  /** Application of a function to a list of vars, and assignment of the result. */
  def :=(f:Function,vs:List[String]): Statement = {
//    println(s"funct (:= def)? $f")
    NFunAssgn(name, vs, f)
  }
//  def :=(f:Function,vs:List[Var]): Statement =
//    NFunAssgn(name,vs.map(v=>v.name),f)//NFunAssgn(toDataVar(name),vs.map(v => toDataVar(v.name)),f)
  /** Guard to check whether the variables belongs to the given predicate. */
  def :< (p:Predicate): Guard = Pred(name,p)//Pred(toDataVar(name),p)
//  def data = toDataVar(name)
//  def flow = toFlowVar(name)
}
//case class IntPred(v:String, p: IntPredicate) extends Guard
case class Pred(dv:String, p:Predicate) extends Guard
case class And(g1: Guard, g2: Guard) extends Guard
case class Or(g1: Guard, g2: Guard) extends Guard
case class Neg(g1: Guard) extends Guard
case class Impl(g1: Guard, g2: Guard) extends Guard
case class Equiv(g1: Guard, g2: Guard) extends Guard
case object True extends Guard
