package picc.examples

import org.junit.Test
import org.junit.Assert._
import picc.DSL._
import picc.connectors.constraints.{Solution, Predicate, Function}

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 15/01/13.
 */

class TestSolCounter {

  @Test def testTransFilter(): Unit = {
    // conn2 has 2 solutions
    assert(transfFilter.doStep.isDefined)
    assert(transfFilter.doStep.isDefined)
    assert(!transfFilter.doStep.isDefined)
  }

  @Test def testTransFilter2(): Unit = {
    // conn2 has 2 solutions
    assert(transfFilter2.doStep.isDefined)
    assert(transfFilter2.doStep.isDefined)
    assert(!transfFilter2.doStep.isDefined)
  }

  @Test def testMrg() {
    // mrg does only one step, and returns 6 on port "sum"
    mrg.doStep match {
      case sol:Solution => assertEquals(sol getDataOn "sum", Some(6))
      case _ => fail("no solution found for mrg")
    }
    assert(!mrg.doStep.isDefined)
  }

  @Test def testFib() {
    // fib can do 3 steps * 5 calculation rounds
    for (i <- 1 to 15)
      assert(fib.doStep.isDefined)
    assert(!fib.doStep.isDefined)
  }


  // connector definitions

  val transfFilter = writer("a",List(4,5)) ++
    transf("a","b", Function("succ"){case x:Int => x+1}) ++
    filter("b","c", Predicate("is5"){case x:Int => x==5})

  val transfFilter2 = writer("a",List(4,5)) ++
    transf("a","b", {x:Int => x+1}) ++
    transf("b","d", (_:Int)+2) ++
    filter("b","c", (_:Int)==5)

  val mrg =
    writer("a",List(4,5)) ++
    writer("b",List(2)) ++
//    transf(List("a","b"),"sum",{x:List[Int]=>x.head+x.tail.head}) ++
    transf(List("a","b"),"sum",mysum) ++ // alternative to above
    reader("sum",2)

  val fib =
    fifo("sum","a") ++
    fifo("a","b",1) ++
    fifo("b","c") ++
    fifo("c","d",0) ++
    transf(List("b","d"),"sum",{x:List[Int]=>x.head+x.tail.head}) ++
    reader("sum",5)


  def mysum = Function("mysum"){
    case n1:Int => {println("A"); n1}
    case List(n1:Int,n2:Int) => {println("AB"); n1 + n2 }
    case List(n1:Int,n2:Int,n3:Int) => {println("ABC"); (n1 + n2) + n3 }
    case x => sys.error("wrong arguments for mysum "+x+":"+x.getClass)
  }




  //  case class Reqq(time: Int, val selection: Int)
//
//  def booking(n:String, search:Function, select:Function, book:Function,
//            cancel:Function, ok:Predicate) =
//    transf(n+"_in",n+"_a",search) ++
//    transf(n+"_a",n+"_b",select) ++
//    transf(n+"_b",n+"_c",book,cancel) ++
//    filter(n+"_c",n+"_out",ok) ++
//    sdrain(n+"_c",n+"_out")
//
//
//  def hotel(id:Int) = booking("h"+id,
//    Function("hsr"+id){case (x:Reqq) => {println("h searching..."+id); x}},
//    Function("hsl"+id){case (x:Reqq) => {println("h selecting..."+id); x}},
//    Function("hb"+id){case (x:Reqq) => {println("h booking..."+id); x}},
//    Function("hc"+id){case (x:Reqq) => {println("h CANCELING..."+id); x}},
//    Predicate("hp"+id){case (x:Reqq) => {println("h checking..."+id);x.selection==id}}
//  )
//
//  def flight(id:Int) = booking("f"+id,
//    Function("fsr"+id){case (x:Reqq) => {println("f searching..."+id); x}},
//    Function("fsl"+id){case (x:Reqq) => {println("f selecting..."+id); x}},
//    Function("fb"+id){case (x:Reqq) => println("f booking..."+id); x},
//    Function("fc"+id){case (x:Reqq) => println("f CANCELING..."+id); x},
//    Predicate("fp"+id){
//      case (x:Reqq) => {
////        println("fffff - is "+x.selection+"=="+id+"? - "+(x.selection==id))
//        println("f checking("+x+")..."+id)
//        readLine()
//        x.selection==id}
//    }
//  )
//
//  val connector =
//    writer("start",List(new Reqq(19,1))) ++
//    exrouter("start","f1_in","f2_in") ++
//    flight(1) ++ flight(2) ++
//    hotel(1) ++ hotel(2) ++ hotel(3) ++
//    exrouter("f1_out","h1_in","h2_in") ++
//    sync("f2_out","h3_in") ++
//    adrain("h1_out","h2_out")

//  println("starting")
//  val s1 = connector.step
//  println("done1 - \n"+s1)//c.getConstraints)
//  val s2 = connector.step
//  println("done2 - \n"+s2)//c.getConstraints)
//  connector.run



  ////////////////////////////////////
  // second connector, in the paper //
  ////////////////////////////////////
//
//  case class Req(val content:String)
//
//  // Define functions
//
//  def srchHotel(i:Int) = Function("SearchHotel-"+i){
//    case r:Req => i match {
//      case 1 => List("F1","Ibis","Mercury")
//      case 2 => List("B&B","YHostel")
//      case _ => List("Aaa","Ibis")
//    }
//  }
//
//  def approve = Predicate("approve"){
//    case l:List[_] =>
//      println("approve: "+l.mkString(", ")+". [y,n]")
//      readLine() == "y"
//    case other =>
//      println("appr: strange type - "+other+" : "+other.getClass)
//      false
//  }
//
//  def book = Function("book"){
//    case l : List[_] =>
//      println("Options: "+l.mkString(", ")+
//        ". Which one? (1.."+l.length+")")
//      val res = readInt()
//      l((res-1)%l.length)
//    case other =>
//      println("book: strange type - "+other+" : "+other.getClass)
//  }
//
////  def cancelB = Function("cancelB"){
////    case (x,_) => println("canceling booking "+x+".") // returns Unit
////  }
//
//  def cancelB = (x: (List[_],_)) =>
//    println("canceling booking: "+x._1.mkString(", ")+".") // returns Unit
//
//
//  def invoice = Function("invoice"){
//    case x => println("sending invoice for "+x+": "+x.getClass) // returns Unit
//  }
//
//  def pay = Predicate("pay"){
//    case x => if (x == "Ibis") {
//      println("paying for Ibis")
//      true
//    }
//    else {
//      println("not paying for "+x)
//      false
//    }
//  }
//
//  val paid = pay
//
//  // the connector:
//  val conn2 =
//    writer("req", List(Req("re1"), Req("req2"))) ++
//    nexrouter("req",List("h1","h2","h3")) ++
//    transf("h1","h1o",srchHotel(3)) ++
//    transf("h2","h2o",srchHotel(2)) ++
//    transf("h3","h3o",srchHotel(1)) ++
//    nmerger(List("h1o","h2o","h3o"),"hs") ++
//    filter("hs","ap",approve) ++
//    sdrain("hs","ap") ++
//    transf("ap","bk",book,cancelB) ++
//    monitor("bk","inv",invoice) ++
//    filter("inv","paid",pay) ++
//    negfilter("inv","npaid",pay) ++
//    reader("paid",5) ++ reader("npaid",5)
//
//  val conn3 =
//    writer("req",List(Req("req1"),Req("req2"))) ++
//    nexrouter("req",List("h1","h2","h3")) ++
//    transf("h1","h1o",srchHotel(1)) ++
//    transf("h2","h2o",srchHotel(2)) ++
//    transf("h3","h3o",srchHotel(3)) ++
//    nmerger(List("h1o","h2o","h3o"),"hs") ++
//    filter("hs","ap",approve) ++
//    // sdrain("hs","ap") ++
//    sdrain("hs","paid") ++
//    transf("ap","bk",book,cancelB) ++
//    monitor("bk","inv",invoice) ++
//    filter("inv","paid",paid) ++
//    reader("paid",5)

//  conn3.run()


//  val approve2 = Predicate("approve2"){
//    case l:String =>
//      println("approve: "+l+". [y,n]")
//      readLine() == "y"
//    case other =>
//      println("appr2: strange type - "+other+" : "+other.getClass)
//      false
//  }
//
//  val testnewsolve = writer("a",List(List("F1","Ibis"))) ++
//      transf("a","b",book) ++ nmerger(List("b","none"),"b2") ++ noflow("none") ++ filter("b2","c",approve2) ++ reader("c",1)
//
//
//  val conn4 = writer("1-h",List(Req("req1"),Req("req2"))) ++
//    nexrouter("1-h",List("11-h1","12-h2","13-h3")) ++
//    transf("11-h1","14-h1o",srchHotel(1)) ++
//    transf("12-h2","15-h2o",srchHotel(2)) ++
//    transf("13-h3","16-h3o",srchHotel(3)) ++
//    nmerger(List("14-h1o","15-h2o","16-h3o"),"2-ho") ++
////    transf("1-h","2-ho",srchHotel(1)) ++
//    filter("2-ho","3-ap",approve) ++  sdrain("2-ho","3-ap") ++
//    transf("3-ap","4-bk",book,cancelB)
//
//
//  val conn5 = writer("1-h",List(Req("r1"))) ++
//    transf("1-h","2-h",srchHotel(1)) ++
//    filter("2-h","3-end",approve) ++
//    reader("3-end",1)
//
//  //  testnewsolve.run()
//  val s = conn3.getConstraints.solveChocoDyn
//  println("---- \n" + s)
////  if (s.isDefined) {
//    println("updating...")
//    conn3.update(s)
////  }
////    conn3.step
  

  
  //// Other experiments: CONCURRENT WORKERS! ////


//  // create and run deployer
//  val deployer = GenDeployer.hybrid(2) // up to 2 workers
//  deployer.start()
//
//  val node_w = deployer.add {
//    writer("req",List(Req("req1"),Req("req2"))) ++
//    nexrouter("req",List("h1","h2","h3"))
//  }
////  val node_t = deployer.add {
////      transf("h1","h1o",srchHotel(1)) ++ filter("h1o","ap",approve) ++ reader("ap",3)
////  }
//  val node_1 = deployer.add {
//    transf("h1","h1o",srchHotel(1))
//  }
//  val node_2 = deployer.add {
//    transf("h2","h2o",srchHotel(2))
//  }
//  val node_3 = deployer.add {
//    transf("h3","h3o",srchHotel(3))
//  }  
//  val node_r = deployer.add {
//    nmerger(List("h1o","h2o","h3o"),"hs") ++
//    filter("hs","ap",approve) ++
//    // sdrain("hs","ap") ++
//    sdrain("hs","paid") ++
//    transf("ap","bk",book,cancelB) ++
//    monitor("bk","inv",invoice) ++
//    filter("inv","paid",paid) ++
//    reader("paid",5)
//  }
//  
//  node_1("h1")  <-- node_w("h1")
//  node_2("h2")  <-- node_w("h2")
//  node_3("h3")  <-- node_w("h3")
//  node_r("h1o") <-- node_1("h1o")
//  node_r("h2o") <-- node_2("h2o")
//  node_r("h3o") <-- node_3("h3o")
//
//  deployer.init


  // PARTIAL part
//
//  val engine = GenEngine.oneStep(2)
//                 // up to 2 workers
//
//  val node_w = engine add (
//    writer("req",List(
//      Req("1"),Req("2"),Req("3"))) ++
//    nexrouter("req",List("s1","s2","s3")) ++
//    srchAppr(1) ++ srchAppr(2) ++ srchAppr(3)
//  )
//  val node_1 = row(1)
//  val node_2 = row(2)
//  val node_3 = row(3)
//
//  def srchAppr(i:Int) =
//    transf("s"+i,"so"+i,srchHotel(i)) ++
//    filter("so"+i,"ap"+i,approve) ++
//    sdrain("so"+i,"ap"+i)
//  def row(i:Int) = engine.add {
//    transf("ap"+i,"bk"+i,book,cancelB) ++
////    transf("ap"+i,"bk"+i,book,(x:Any) => println("canceling: "+x)) ++
//    monitor("bk"+i,"inv"+i,invoice) ++
//    filter("inv"+i,"paid"+i,paid) ++
//    sdrain("inv"+i,"paid"+i) ++
//    reader("paid"+i,5)
//  }
//
//  // connecting ports from different nodes
//  node_1("ap1")  <-- node_w("ap1")
//  node_2("ap2")  <-- node_w("ap2")
//  node_3("ap3")  <-- node_w("ap3")
//
//  // starting the algorigthm
//  engine.init


}
