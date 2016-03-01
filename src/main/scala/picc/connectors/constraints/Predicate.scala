package picc.connectors.constraints

/**
 * Unary predicate that is embedded in the [[picc.connectors.constraints.Constraint]].
 * Predicate channels use these predicates in their constraints.
 *
 * Created by jose on 13/07/12.
 */
abstract class Predicate {
  /** Checks if the predicate holds for a given `x`. */
  def check(x: Any): Boolean

  //  def opposite = new UnPred {
  //    def check(x: Any) = this.check(x)
  //  }
}

object Predicate {
  /**
   * Constructs a [[picc.connectors.constraints.Predicate]] from a scala partial [[scala.Function1]].
   * Typically the function is a block of `case x: Type => ...`. If no case matches it outputs `false`.
   * @param body is the scala [[scala.Function1]].
   * @return new [[picc.connectors.constraints.Predicate]] that can be embedded in the synchronous constraints.
   */
  def apply[A]()(body: A => Boolean): Predicate = new Predicate {
    def check(x: Any) =
      try x match {case y: A @unchecked => body(y) }
      catch {
        case e: scala.MatchError => false    // return false if undefined
        case e: java.lang.ClassCastException => false    // return false if undefined
        case e: Throwable => throw e
        // note: 'A' is lost at runtime, so the matchError does not work.
      }
//      try body(x)
//      catch {
//        case e: scala.MatchError => false
//        case e => throw e
//      }
  }

  /**
   * Same as [[picc.connectors.constraints.Predicate.apply()]] with a redefined name as `toString`.
   * @param name is the new `toString` value.
   * @param body is the scala [[scala.Function1]]
   * @return the new [[picc.connectors.constraints.Predicate]].
   */
  def apply[A](name: String)(body: Any => Boolean): Predicate = new Predicate {
    def check(x: Any) =
      try x match {case y:A @unchecked => body(y) }
      catch {
        case e: scala.MatchError => {println(s"FAIL (match error by predicate) - unexpected $x:${x.getClass}");false }
        case e: java.lang.ClassCastException => println(s"FAIL (match error by predicate) - unexpected $x:${x.getClass}");false
        case e:Throwable => {println("FAIL");throw e}
        // note: 'A' is lost at runtime, so the matchError does not work.
      }
    override def toString = name
  }
//  def apply(name: String)(body: Any => Boolean): Predicate = new Predicate {
//    def check(x: Any) = try body(x)
//    catch {
//      case e: scala.MatchError => {println("FAIL (match error by predicate)");false }
//      case e => {println("FAIL");throw e}
//    }
//    override def toString = name
//  }

  implicit def func2Pred[A](body:A=>Boolean): Predicate =
    new Predicate{
      def check(x: Any) = {
        try x match { case y: A @unchecked => body(y) }
        catch {
          case e: scala.MatchError => false  // return false if undefined
          case e: java.lang.ClassCastException => false    // return false if undefined
          case e: Throwable => throw e
          // note: 'A' is lost at runtime, so the matchError does not work.
        }
      }
      override def toString = "anon-"+body.hashCode()
    }
}

