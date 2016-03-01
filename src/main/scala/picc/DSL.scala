package picc

import picc.connectors.constraints._
import picc.connectors.primitives._

/**
  * Created by jose on 26/02/16.
  */
object DSL {

  def adrain(a: String, b: String) = new ADrain(a,b)
  def fifo(a: String, b: String, data:Any) = new Fifo(a,b,Some(data))
  def fifo(a: String, b: String) = new Fifo(a,b,None)
//  def sfifo(a: String, b: String, data:Option[AnyRef]) = new SyncFifo(a,b,data)
  // filters!
  //  def filter(a: String, b: String, g:Guard) = new Filter(a,b,g)
  def filter(a: String, b: String, filter:Predicate) = new Filter(a,b,filter)
//  def filter[A](a: String, b: String, filter: (A) => Boolean) = new TFilter(a,b,filter)
  def negfilter(a: String, b: String, filter:Predicate) = new Filter(a,b,filter,false)
//  def sfilter(a: String, b: String, filter:Predicate) = new SFilter(a,b,filter)
  /////
//  def imerger(a: String, b: String, c: String) = new IMerger(a,b,c)
  def merger(a: String, b: String, c: String) = new NMerger(List(a,b),c)
  def nmerger(srcs: List[String], snk: String) = new NMerger(srcs,snk)
  def lossy(a: String, b: String) = new Lossy(a,b)
  def sdrain(a: String, b: String) = new SDrain(a,b)
//  def sspout(a: String, b: String) = new SSpout(a,b)
  def sync(a: String, b: String) = new Sync(a,b)
  // transformers!
  def transf(a: String, b: String, f: Function) = new Transf(a,b,f)
  def transf(a: String, b: String, f: Function, undo: Function) = new TransfUndo(a,b,f,undo)
  def transf(as: List[String], b: String, f: Function) = new NTransf(as,b,f)
//  /* using (scala's) partial functions */
//  def transf(a: String, b: String, f: PartialFunction[Any,Any]) = new PTransf(a,b,f)
  //  def transf[A](a: String, b: String, f: PartialFunction[A,_]) = new PTransf(a,b,f)
  // using (scala's) total funtions
//  def transf[A](a: String, b: String, f: (A) => _) = new TTransf[A](a,b,f)
  /////
  def monitor(a: String, b: String, f: Function) = new Monitor(a,b,f)
  def exrouter(a: String, b: String, c: String) = new ExRouter(a,b,c)
  def nexrouter(src: String, snks: List[String]) = new NExRouter(src,snks)
  def reader(a: String,n: Int) = new Reader(a,n)
  def reader(a: String) = new Reader(a,-1)
  def writer(a: String, data: List[Any]) = new Writer(a,data)
//  def jwriter(a: String, data: java.util.List[Any]) =
//    new Writer(a,data.toArray.toList)
//
//  def flow(a: String) = new Connector(List(a)) {
//    def getConstraints = Formula(True --> a)//Var(Utils.flowVar(a,getID)))
//  }
//  def noflow(a: String) = new Connector(List(a)) {
//    def getConstraints = Formula(True --> !a)//Neg(Var(Utils.flowVar(a,getID))))
//  }
//  def empty = new Connector(List()) {
//    def getConstraints = Formula()


  implicit def str2var(s:String) = Var(s)
  implicit def st2GC(s: Statement): GuardedCommand = GuardedCommand(True,s)
  implicit def gc2GCs(gc: GuardedCommand): Constraint= Constraint(gc)
  implicit def st2GCs(s: Statement): Constraint = Constraint(GuardedCommand(True,s))

//  implicit def func2Pred(f:Any => Any): Predicate = Predicate("anon-"+f.hashCode()){case x => f(x)==true}
//  implicit def func2Func(f:Any => Any): Function= Function("anon-"+f.hashCode()){case x => f(x)}

}
