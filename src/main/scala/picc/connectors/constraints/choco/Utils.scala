package picc.connectors.constraints.choco

import _root_.choco.Choco
import _root_.choco.kernel.model.variables.integer.IntegerVariable
import _root_.choco.kernel.solver.constraints.Formula
import picc.connectors.constraints._
import picc.connectors.constraints.choco.Solver.VarMap
import picc.DSL._

import scala.collection.mutable.ListBuffer

/**
  * Created by jose on 27/02/16.
  */
object Utils {

  /**
    * Retrieves an IntegerVariable from a map if it exists, or creates one if it does not exist, updating the map.
    *
    * @param m map from variable names (Strings) to Choco variables (IntegerVariables)
    * @param name of the variable (String)
    * @return Choco variable (IntegerVariable) for the given variable name
    */
  def getVar(m: VarMap, name: String, isNat:Boolean): IntegerVariable = {
    val v: String = if (isNat) mkDataVar(name) else mkFlowVar(name)
    if (m contains v)
      m(v)
    else if (isNat) {
      val chocov = Choco.makeIntVar(v) //,-4294967296,4294967296)
      m += ((v , chocov))
      chocov
    }
    else {
      // is boolean var
      val chocov = Choco.makeBooleanVar(v)
      m += ((v , chocov))
      chocov
    }
  }

  def optimChocoVars(gcs: Constraint, vars: VarMap) {
    for (gc <- gcs.commands)
      if (gc.g == True)
        optimChocoVars(gc.st,vars)
    //    vars
  }

  private def optimChocoVars(s: Statement,vars: VarMap): Unit = s match {
    case VarAssgn(v1, v2) =>
      if (vars contains mkDataVar(v1))
        vars += ((mkDataVar(v2) , vars(mkDataVar(v1))))
      else {
        val v2var = getVar(vars,v2,isNat = true)
        vars += ((mkDataVar(v1) , v2var))
      }
    case _: Guard =>
    case IntAssgn(v, d) =>
    case FunAssgn(v1, v2, f) =>
    case NFunAssgn(v1, vs, f) =>
    case DataAssgn(v, d) =>
    case Seq(Nil) =>
    case Seq(st::sts) => {optimChocoVars(st,vars);optimChocoVars(Seq(sts),vars)}
  }

  /**
    * Add SOME-FLOW constraints.
    * CC3 not in use - it should be done here if CC3 is added.
    * Used before using the Choco solver.
    */
  def close(c:Constraint): Constraint = {
    //    println("## closing...")
    //      println("## not closed yet...")
    val bvars = new ListBuffer[String]()
    c.fv(bvars)
    //      println("## b vars: "+bvars.size)
    if (bvars.nonEmpty) {
      var comms = Set[GuardedCommand]()
      val fst = bvars.head
      var flowConstraint: Guard = Var(fst)
      for (v <- bvars.tail)
        flowConstraint = flowConstraint \/  Var(v)
      //        println("closing - adding "+flowConstraint)
      comms += flowConstraint
      Constraint(comms ++ c.commands)
    }
    else c
  }



  def mkFlowVar(x: String): String = "F€" + x
  def mkDataVar(x: String): String = "V€" + x
  def mkPredVar(v: String, pred: Any, fs: List[Any]) = v + "#" + pred + "_" + fs.mkString(".")//.hashCode()

  def ppVar(x:String) = if (x.startsWith("F€")) ppFlowVar(x) else ppDataVar(x)
  def ppFlowVar(x:String): String = { val y = x.split("€"); placeIndex(y) }
  def ppDataVar(x:String): String = { val y = x.split("€"); "^"+placeIndex(y) }
  def ppPredVar(x:String): String = { val y = x.split("€"); "P_"+placeIndex(y) }
  private def placeIndex(y:Array[String]) =
    if (y.size > 2 && y(2).size >5) {
      if (y(2) == "0") y(1) else y(1)+"["+y(2).substring(6)+"]"
    }
    else if (y.size > 1) y(1) else ""

}
