package picc.connectors.primitives

import picc.connectors.constraints.{Solution, OptSolution, Function}

/**
  * Created by jose on 01/03/16.
  */
class Monitor(a: String, b: String, f: Function) extends Sync(a,b) {

  override def update(s: OptSolution) {
    //    println("updating! - based on "+a+"\n"+s.get)
    //    println("s.get of "+a+": "+s.get.getDataOn(dataVar(a,uid)))
    super.update(s)
    s match {
      case sol:Solution => if (sol.hasFlowOn(a) && sol.getDataOn(a).isDefined)
        f.calculate(sol.getDataOn(a).get)
      case _ => {}
    }
  }
}
