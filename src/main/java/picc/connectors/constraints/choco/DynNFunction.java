package picc.connectors.constraints.choco;

import choco.cp.solver.variables.integer.IntVarEvent;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.AbstractSConstraint;
import choco.kernel.solver.constraints.SConstraintType;
import choco.kernel.solver.constraints.integer.AbstractIntSConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;
import picc.connectors.constraints.Buffer;
import picc.connectors.constraints.Function;
import scala.collection.immutable.Nil$;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * <p/>
 * Created by jose on 10/04/13.
 */
public class DynNFunction extends AbstractIntSConstraint {

    final DataMap dm;
    final Buffer buffer;
    final Function function;

    // x = f(y1,y2,..)  --->  x,^x,y1,^y1,y2,^y2,...
    public DynNFunction( IntDomainVar[] vars,
                        DataMap data,
                        Buffer buffer,
                        Function function) {
        super(4,vars);  // priority 1 - 4: 1 more active with fast propagation, 4 slower/delayed listeners
        this.dm = data;
        this.buffer = buffer;
        this.function = function;
    }

    @Override
    public int getFilteredEventMask(int idx) {
        return IntVarEvent.INSTINT_MASK;
    }

    /** * Default initial propagation: full constraint re-propagation. */
    public void awake() throws ContradictionException {
        propagate();
    }

    /**
     * default propagation on instantiation: full constraint re-propagation
     * @param idx index of the variable to reduce
     */
    public void awakeOnInst(int idx) throws ContradictionException {
        propagate();
    }

    /** * <i>Propagation:</i> * Propagating the constraint until local consistency is reached. * * @throws ContradictionException * contradiction exception */

    public void propagate() throws ContradictionException {
//        System.out.print("# N-PROPAGATING (x = f(y)) - "+getVar(1).getName()+" = f(...) - ");
//        for (int i=0; i<getNbVars(); i++)
//            System.out.print(getVar(i)+" ");
//        System.out.println("");

        // ^x := f (^yn) -- if x,yn,^yn instantiated, x,yn have flow, instantiate ^x

        // x instantiated
        if(!getVar(0).isInstantiated()) return;
        // each yn and ^yn is instantiated
        for (int i=2; i<getNbVars(); i++)
            if (! getVar(i).isInstantiated()) return;
        // x and each yn has flow (== 1)
        for (int i=0; i<getNbVars(); i+=2)
            if (getVar(i).getVal() == 0) return;

        // calculate function value
        // order of objects seems to be right!
//        List<Object> l = new ArrayList<Object>();
//        for (int i=3; i<getNbVars(); i+=2)
//            l.add(dm.get(getVar(i).getVal()));

        scala.collection.immutable.List sl = Nil$.MODULE$; // create empty list
        for (int i=3; i<getNbVars(); i+=2)
            sl = sl.$colon$colon(dm.get(getVar(i).getVal()));


        Object newVal = buffer.calculate(function,sl.reverse());

//        System.out.println("calculated "+function+"("+sl+") = "+newVal+" based on inputs");
//        System.out.println("dm: "+dm+"  - getVal("+getVar(1)+")="+getVar(1).getVal());

        if (getVar(1).isInstantiated()) { // && dm.get(getVar(1).getVal()) != null
//                System.out.println("already defined as "+dm.get(getVar(1).getVal())+"...");
            if (dm.get(getVar(1).getVal()) != newVal) fail();
        }
        else {
//            getVar(1).setVal(dm.add(newVal));
//            System.out.println("defined "+getVar(1).getName()+" as "+newVal);
            IntDomainVar x = getVar(1);
//            System.out.println("-1-"+x);
            Integer i = dm.add(newVal);
//            System.out.println("-2-"+i);
            x.setVal(i);
//            System.out.println("-3-");
//            System.out.println("newDM "+dm);
        }
    }

    /**
     * Checks if the constraint is satisfied when the variables are instantiated.
     */
    @Override
    public boolean isSatisfied(int[] tuple) {
//        System.out.print("# N-IsSatisfied Func "+function+"? - "+getVar(1).getName()+" = f(...) - ");
//        for (int i=0; i<getNbVars(); i++)
//            System.out.print(getVar(i)+" ");
//        System.out.println("");


        // x = F or y = F or ^x = f (^y)
        for (int i=0; i<getNbVars(); i+=2)
            if (tuple[i] == 0) return true;

//        List<Object> l = new ArrayList<Object>();
//        for (int i=3; i<getNbVars(); i+=2)
//            l.add(dm.get(tuple[i]));
        scala.collection.immutable.List sl = Nil$.MODULE$; // create empty list
        for (int i=3; i<getNbVars(); i+=2)
            sl = sl.$colon$colon(dm.get(getVar(i).getVal()));

        Object newVal = buffer.calculate(function,sl);
        return (dm.get(tuple[1]) == newVal);
    }


    public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
//        throw new RuntimeException("Lazy constraints can occur only in positive positions!");
//        DEBUG("### OPOSITE");
        return new NegDynNFunction(this);
    }

//    public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
////        throw new RuntimeException("Lazy constraints can occur only in positive positions!");
//        return new NegLazyPredSConstraint(v0,v1,v2,data,buffer,predicate,functions);
//    }

    public String pretty() {
        return function.toString()+ "("+getVar(0).getName()+")";
    }


    private class NegDynNFunction extends AbstractSConstraint<IntDomainVar> {

        final DynNFunction hc;

        public NegDynNFunction(DynNFunction hc) {
            super(hc.vars);
            this.hc = hc;
//            DEBUG("### CREATING NEGATION");
        }

        public void propagate() throws ContradictionException {
            throw new RuntimeException("negation of a DynNFunction should NOT be propagated!");
        }

        @Override
        public boolean isConsistent() {
            throw new RuntimeException("negation of a DynNFunction should NOT be checked for isConsistent!");
        }

        public boolean isSatisfied() {
            throw new RuntimeException("negation of a DynNFunction should NOT be checked for isSatisfied!");
        }

        public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
//        throw new RuntimeException("Lazy constraints can occur only in positive positions!");
            return hc;
        }

        public SConstraintType getConstraintType() {
            throw new RuntimeException("negation of a DynPredicate should NOT be asked for its Constraint Type!");
        }

    }

}