package picc.connectors.constraints

//import java.util.List
import scala.collection.JavaConversions._
import scala.collection.immutable.Map


/**
 * Caching mechanism for [[Function]]'s and [[Predicate]]'s:
 * each function and predicate is evaluated with the same argument only once,
 * and the results are buffered.
 *
 * User: jose
 * Date: 1/03/16
 */

class Buffer {
//  println("buf: "+hashCode)
  protected var calculatedP: Map[(Predicate,Any),Boolean] = Map()
  protected var calculatedF: Map[(Function,Any),Any] = Map()


  /**
   * Calculate the result of a sequence function using a *caching* mechanism
   * @param functs List of functions f1, f2, .. fn.
   * @param d Data value to be passed to the first function
   * @return fn(..(f2(f1(d))))
   */
  def calculate(functs:List[Function],d:Any): Any = functs match {
    case Nil => d
    case (f: Function)::(fs: List[Function]) =>
      calculate(fs,calculate(f,d))
  }

  /**
   * Calculate the result of function using a *caching* mechanism
   * @param funct Functions f to be applied
   * @param d Data value to be passed to the f
   * @return f(d)
   */
  def calculate(funct: Function, d:Any): Any =
    if (calculatedF contains (funct,d)) {
//              println("# buffered func #")
      calculatedF((funct,d))
    }
    else {
//      println("-- "+funct)
      val res = funct.calculate(d)
//              println("# adding "+funct+"("+d+") -> "+res+" to buffer "+hashCode())
      calculatedF += (funct,d) -> res
//              print("# Calc func - "+res+" ")
      res
    }


  /**
   * Apply functions and then a predicate to a data value, using a *caching* mechanism
   * @param p Predicate to be evaluated
   * @param fs List of functions  f1, f2, ..., fn
   * @param d Dat1a value to be passed to the first function
   * @return p(fn(..(f2(f1(d)))))
   */
  def check(p:Predicate, fs:java.util.List[Function], d:Any) = {
//    println("#### checking "+p+"*"+fs.reverse.mkString(".")+"*"+d+"... ")
    val newd = calculate(asJavaIterable(fs).toList.reverse,d)
    calculatedP.get((p, newd)) match {
      case Some(x) =>
//        println("(buffered)")
        if (x) 1 else 0
      case None =>
        val res = p.check(newd)
//        println("# adding "+p+"("+newd+") -> "+res+" to buffer")
        calculatedP += (p,newd) -> res
//        println("# Calc P - "+res+" ####")
        if (res) 1 else 0
    }
  }

  /**
   * Apply a predicate to a data value, using a *caching* mechanism
   * @param p Predicate to be evaluated
   * @param d Dat1a value to be passed to the first function
   * @return p(fn(..(f2(f1(d)))))
   */
  def check(p:Predicate, d:Any) = {
    //    println("#### checking "+p+"*"+fs.reverse.mkString(".")+"*"+d+"... ")
    calculatedP.get((p, d)) match {
      case Some(x) =>
        //        println("(buffered)")
        if (x) 1 else 0
      case None =>
        val res = p.check(d)
        //        println("# adding "+p+"("+newd+") -> "+res+" to buffer")
        calculatedP += (p,d) -> res
        //        println("# Calc P - "+res+" ####")
        if (res) 1 else 0
    }
  }

  /**
   * Apply 'undo' to data 'd' for every calculation of 'f'(d) except if 'd'='data'.
   * Not optimised - Iterating over all buffered applications of functions.
   * (We could modify the 'calculatedF' map to be a nested map to avoid iteration.)
   * @param f function to be reverted
   * @param undo reverting function
   * @param arg possible successful data used as argument, that must not be reverted
   */
  def rollback(f: Function, undo: Function, arg: Option[Any]) {
//    print("rollbacking "+f+"("+data+") with "+undo)
//    println(" @"+hashCode+calculatedF.keys.mkString("[",",","]"))
    for (((f2,d),dres) <- calculatedF) {
//      print(" - "+f2+"("+d+")")
      if (f2 == f) {
        if (arg.isEmpty || (arg.get != d)) {
//          println(" *** succeeded - rollingback ***" +f+"("+d+") = "+ dres+" - except "+data)
          undo.calculate(d,dres)
        }
//        else println(" *** succeeded - not rollingback ***")
      }
//      else println(" *** skipping - wrong function ***")
    }
//    println(" done.")
  }

  
  /**
   * Imports another buffer in a conservative approach:
   * if a value is defined and different in both, ignore it (to be recalculated).
   */
  def safeImport(other:Buffer) {
    //println("importing other buffer "+other)
    for ((f,r) <- other.calculatedF)
      if (calculatedF contains f) {
        if (calculatedF.get(f) != Some(r)) calculatedF -= f
      } 
      else calculatedF += (f -> r)
    for ((p,r) <- other.calculatedP)
      if (calculatedP contains p) {
        if (calculatedP.get(p) != Some(r)) calculatedP -= p
      } 
      else calculatedP += (p -> r)
  }
}
