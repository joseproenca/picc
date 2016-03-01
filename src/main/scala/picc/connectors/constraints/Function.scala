package picc.connectors.constraints

import scala.collection.JavaConverters._

/**
 * Unary function that is embedded in the [[picc.connectors.constraints.Constraint]].
 * Transformer channels use these functions in their constraints.
 *
 * Created by jose on 16/07/12.
 */
abstract class Function {
  /** Applies the (untyped) function to a value `x`. */
  def calculate(x: Any): Any
}

object Function {
  /** Constructs a [[picc.connectors.constraints.Function]] from a scala partial [[scala.Function1]].
    * Typically the function is a block of `case x: Type => ...`. If no case matches it outputs ``():Unit.
    * @param body is the scala [[scala.Function1]].
    * @return new [[picc.connectors.constraints.Function]] that can be embedded in the synchronous constraints.
    */
  def apply[A]()(body: A => Any): Function =
    new Function {
      def calculate(x: Any) = {
        // calculate is called within a Java program, that cannot generate scala lists properly... (!)
//        x match {
//          case jl: java.util.List[Any] => input = jl.asScala.toList
//          case _ => {}
//        }
        try x match { case y: A @unchecked => body(y) }
        catch {
          case e: scala.MatchError => {}    // return Unit if undefined
          case e: java.lang.ClassCastException => {}    // return Unit if undefined
          case e: Throwable => throw e
//          case e: Throwable => {println("ERROR in calc"); throw e}
          // note: 'A' is lost at runtime, so the matchError does not work.
        }
      }
    }

  /**
   * Same as [[picc.connectors.constraints.Function.apply()]] with a redefined name as `toString`.
   * @param name is the new `toString` value.
   * @param body is the scala [[scala.Function1]]
   * @return the new [[picc.connectors.constraints.Function]].
   */
  def apply(name:String)(body: Any => Any): Function =
    new Function {
      def calculate(x: Any) = {
//        val input = x
//        x match {
//          case jl: java.util.List[Any] => input = jl.asScala.toList
//          case _ => {}
//        }
        try x match { case y: Any @unchecked => body(y) }
        catch {
          case e: scala.MatchError => {}  // return Unit if undefined
          case e: java.lang.ClassCastException => {}    // return Unit if undefined
//          case e: Throwable => {println(s"ERROR in $name($x:${x.getClass}) "); throw e}
          case e: Throwable => throw e
          // note: 'A' is lost at runtime, so the matchError does not work.
        }
      }

      override def toString = name
    }
  
  /**
   * Allows Scala functions to be implicitly converted to Functions.
   * Makes definition of transformer channels easier.
   */
  implicit def func2Func[A,B](body:A=>B): Function =
    new Function {
      def calculate(x: Any) = {
        try x match { case y: A @unchecked => body(y) }
        catch {
          case e: scala.MatchError => {}  // return Unit if undefined
          case e: java.lang.ClassCastException => {}    // return Unit if undefined
          case e: Throwable => throw e
          // note: 'A' is lost at runtime, so the matchError does not work.
        }
      }
  }

}