package picc.connectors.constraints.choco;

import choco.cp.solver.variables.integer.IntVarEvent;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.AbstractSConstraint;
import choco.kernel.solver.constraints.SConstraintType;
import choco.kernel.solver.constraints.integer.AbstractIntSConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;
import picc.connectors.constraints.Buffer;
import picc.connectors.constraints.Function;
import picc.connectors.constraints.Predicate;

/**
 * Created with IntelliJ IDEA.
 * <p/>
 * Created by jose on 05/04/13.
 */
public class DynFunction extends AbstractIntSConstraint {

    final DataMap dm;
    final Buffer buffer;
    final Function function;

    public DynFunction( IntDomainVar xvar,
                        IntDomainVar xdata,
                        IntDomainVar yvar,
                        IntDomainVar ydata,
                        DataMap data,
                        Buffer buffer,
                        Function function) {
        super(4,new IntDomainVar[]{xvar, xdata, yvar, ydata});
        // priority 1 - 4: 1 more active with fast propagation, 4 slower/delayed listeners
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
//        if (idx == 0 || idx == 1 ) {
////            System.out.println("# AWAKE (Pred) - xflow instantiated! - "+v1.getVal());
//            propagate();
//        }
//        if (idx == 1) { propagate(); }
    }

    /** * <i>Propagation:</i> * Propagating the constraint until local consistency is reached. * * @throws ContradictionException * contradiction exception */

    public void propagate() throws ContradictionException {
//        System.out.println("# PROPAGATING (x = f(y)) - "+getVar(1).getName()+" = f("+getVar(3).getName()+") - x? ^x? y? ^y? "+
//                getVar(0).isInstantiated()+" "+getVar(1).isInstantiated()+" "+getVar(2).isInstantiated()+" "+getVar(3).isInstantiated());

        // ^x := f (^y) -- if x,y,^y instantiated, x,y have flow, instantiate ^x
        if(getVar(0).isInstantiated() && getVar(2).isInstantiated() && getVar(3).isInstantiated())
            if(getVar(0).getVal() == 1 && getVar(2).getVal() == 1) {
                Object newVal = buffer.calculate(function,dm.get(getVar(3).getVal()));
//                System.out.println("defining "+getVar(1).getName()+" from f -> "+newVal+" -> idx "+getVar(3).getVal()+"");
                if (getVar(1).isInstantiated()) {
//                    System.out.println("already defined as "+dm.get(getVar(1).getVal())+"...");
                    if (dm.get(getVar(1).getVal()) != newVal) fail();
                }
                else
                    getVar(1).setVal(dm.add(newVal));
            }
    }

    /**
     * Checks if the constraint is satisfied when the variables are instantiated.
     */
    @Override
    public boolean isSatisfied(int[] tuple) {
//        System.out.println("# IsSatisfied Func? ("+getVar(0).getName()+" - "+getVar(1).getName()+") - called");
        // x = F or y = F or ^x = f (^y)
        if (tuple[0] == 0 || tuple[2] == 0) return true;
        else {
            Object newVal = buffer.calculate(function,dm.get(tuple[3]));
            return (dm.get(tuple[1]) == newVal);
        }
    }


    public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
//        throw new RuntimeException("Lazy constraints can occur only in positive positions!");
//        DEBUG("### OPOSITE");
        return new NegDynFunction(this);
    }

//    public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
////        throw new RuntimeException("Lazy constraints can occur only in positive positions!");
//        return new NegLazyPredSConstraint(v0,v1,v2,data,buffer,predicate,functions);
//    }

    public String pretty() {
        return function.toString()+ "("+getVar(0).getName()+")";
    }


    private class NegDynFunction extends AbstractSConstraint<IntDomainVar> {

        final DynFunction hc;

        public NegDynFunction(DynFunction hc) {
            super(hc.vars);
            this.hc = hc;
//            DEBUG("### CREATING NEGATION");
        }

        public void propagate() throws ContradictionException {
            throw new RuntimeException("negation of a DynFunction should NOT be propagated!");
        }

        @Override
        public boolean isConsistent() {
            throw new RuntimeException("negation of a DynFunction should NOT be checked for isConsistent!");
        }

        public boolean isSatisfied() {
            throw new RuntimeException("negation of a DynFunction should NOT be checked for isSatisfied!");
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