package picc.examples

import picc.DSL._
import picc.connectors.constraints.{Predicate, Function}

/**
  * Created by jose on 29/02/16.
  */
object HotelReservation extends App {
  case class Req(val content:String)

  // Define functions

  def srchHotel(i:Int) = Function("SearchHotel-"+i){
    case r:Req => i match {
      case 1 => List("F1","Ibis","Mercury")
      case 2 => List("B&B","YHostel")
      case _ => List("Aaa","Ibis")
    }
  }

  def approve = Predicate("approve"){
    case l:List[_] =>
      println("approve: "+l.mkString(", ")+". [y,n]")
      readLine() == "y"
    case other =>
      println("appr: strange type - "+other+" : "+other.getClass)
      false
  }

  def book = Function("book"){
    case l : List[_] =>
      println("Options: "+l.mkString(", ")+
        ". Which one? (1.."+l.length+")")
      val res = readInt()
      l((res-1)%l.length)
    case other =>
      println("book: strange type - "+other+" : "+other.getClass)
  }

  //  def cancelB = Function("cancelB"){
  //    case (x,_) => println("canceling booking "+x+".") // returns Unit
  //  }

  def cancelB = (x: (List[_],_)) =>
    println("canceling booking: "+x._1.mkString(", ")+".") // returns Unit


  def invoice = Function("invoice"){
    case x => println("sending invoice for "+x+": "+x.getClass) // returns Unit
  }

  def pay = Predicate("pay"){
    case x => if (x == "Ibis") {
      println("paying for Ibis")
      true
    }
    else {
      println("not paying for "+x)
      false
    }
  }

  val paid = pay

  // the connector:
  val conn1 = // results in pay or not-pay
    writer("req", List(Req("re1"), Req("req2"))) ++
      nexrouter("req",List("h1","h2","h3")) ++
      transf("h1","h1o",srchHotel(3)) ++
      transf("h2","h2o",srchHotel(2)) ++
      transf("h3","h3o",srchHotel(1)) ++
      nmerger(List("h1o","h2o","h3o"),"hs") ++
      filter("hs","ap",approve) ++
      sdrain("hs","ap") ++
      transf("ap","bk",book,cancelB) ++
      monitor("bk","inv",invoice) ++
      filter("inv","paid",pay) ++
      negfilter("inv","npaid",pay) ++
      reader("paid",5) ++ reader("npaid",5)

  val conn2 = // require payment - cancel unpaid bookings in the end.
    writer("req",List(Req("req1"),Req("req2"))) ++
      nexrouter("req",List("h1","h2","h3")) ++
      transf("h1","h1o",srchHotel(1)) ++
      transf("h2","h2o",srchHotel(2)) ++
      transf("h3","h3o",srchHotel(3)) ++
      nmerger(List("h1o","h2o","h3o"),"hs") ++
      filter("hs","ap",approve) ++
      // sdrain("hs","ap") ++
      sdrain("hs","paid") ++
      transf("ap","bk",book,cancelB) ++
      monitor("bk","inv",invoice) ++
      filter("inv","paid",paid) ++
      reader("paid",5)

  conn1.run()

  conn2.run()
}
