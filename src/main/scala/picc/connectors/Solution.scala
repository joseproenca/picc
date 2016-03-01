package picc.connectors.constraints

import _root_.choco.cp.solver.CPSolver
import _root_.choco.cp.solver.constraints.reified.leaves.bool.FalseNode
import _root_.choco.kernel.model.variables.integer.IntegerVariable
import picc.connectors.constraints.choco.DataMap

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 26-02-2016.
 */

sealed abstract class OptSolution(b:Buffer) {
  def getBuffer: Buffer = b
  val isDefined: Boolean
}

 
class Solution(choSol: CPSolver, varMap:Map[String,IntegerVariable],
                   b: Buffer, datamap:DataMap)
extends OptSolution(b) {
  val isDefined=true

  def getDataOn(v: String): Option[Any] = {
    val dv = choco.Utils.mkDataVar(v)
    if (varMap contains dv)
      getDataOn(varMap(dv))
    else
      None
  }

  private def getDataOn(v: IntegerVariable): Option[Any] =
    if (choSol contains v) {
      val idx = choSol.getVar(v).getVal
      if (idx > 1) {
        val res = datamap.get(idx)
        Option(res)
      }
      else
        Some(idx)
    }
    else None


  def hasFlowOn(v: String) = {
//        println("has flow on '"+v+"'? - getVal: "+ getDataOn(v))
    val sv = choco.Utils.mkFlowVar(v)
    if (varMap contains sv)
      getDataOn(varMap(sv)) == Some(1)
    else
      false
  }

  private def size = varMap.size
  private def sizeModel = choSol.getModel.getNbIntVars

//   override def withID(id:Int) =
//     new DynSolution(choSol, varMap,b,datamap,newpred) {
//       override def getDataOn(end:String) = super.getDataOn(addID(end,id))
//     }

  override def toString: String = {
    var res: String = ""

    for ((k,v) <- varMap.toList.sortBy((x:(String,IntegerVariable)) => x._1)) {
      val dt = getDataOn(v)
      if (dt.isDefined) {
        dt.get match {
          case i: Int if i <= -21474830 => res += choco.Utils.ppVar(k) + " -> " + "-any-\n"
          case _                        => res += choco.Utils.ppVar(k) + " -> " + dt.get + "\n"
        }
      }
      else
        res += choco.Utils.ppVar(k) + " -> Undefined\n"
    }
    res
  }

  def apply(v:String): Any =
    getDataOn(v) match {
      case Some(x) => x
      case None => 0
    }
}


class NoSolution(b:Buffer) extends OptSolution(b) {
  val isDefined=false
  override def toString: String = "NoSolution"
}