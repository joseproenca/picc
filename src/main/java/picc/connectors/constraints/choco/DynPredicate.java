package picc.connectors.constraints.choco;

import choco.cp.solver.variables.integer.IntVarEvent;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.AbstractSConstraint;
import choco.kernel.solver.constraints.SConstraintType;
import choco.kernel.solver.constraints.integer.AbstractTernIntSConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;
import picc.connectors.constraints.Buffer;
import picc.connectors.constraints.Predicate;

import java.util.ArrayList;

/**
 * Created with IntelliJ IDEA.
 * <p/>
 * Created by jose on 05/04/13.
 */
public class DynPredicate extends AbstractTernIntSConstraint {

    final DataMap dm;
    final Buffer buffer;
    final Predicate predicate;

    public DynPredicate(IntDomainVar xvar,
                               IntDomainVar xdata, // for information, not for restriction
                               IntDomainVar xpred, // for information, not for restriction
                               DataMap data,
                               Buffer buffer,
                               Predicate predicate) {
        super(xvar,xdata,xpred);
        this.dm = data;
        this.buffer = buffer;
        this.predicate = predicate;
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
        if (idx == 0) {
//            System.out.println("# AWAKE (Pred) - xflow instantiated! - "+v1.getVal());
            propagate();
        }
        if (idx == 1) { propagate(); }
    }

    /** * <i>Propagation:</i> * Propagating the constraint until local consistency is reached. * * @throws ContradictionException * contradiction exception */

    public void propagate() throws ContradictionException {
//        System.out.println("# PROPAGATING (P(x)) - "+v2.getName()+"("+v1.getName()+") - x? ^x? Px? "+
//                getVar(0).isInstantiated()+" "+getVar(1).isInstantiated()+" "+getVar(2).isInstantiated());

        if(v0.isInstantiated() && v1.isInstantiated())
            if(v0.getVal() == 1) {
            	if (dm.get(v1.getVal())==null)
            		// x^ has a value, but it is not a reference - hence predicate yields false!
            		v2.removeVal(1, this, false);
//                System.out.println("defining (P(x)) - "+v1.getName()+" = "+dm.get(v1.getVal())+" (idx "+v1.getVal()+")");
                        // NOTE: dm.get can return null, if v1.getVal() has randomly assigned trash.
            	else if (buffer.check(predicate,new ArrayList(),dm.get(v1.getVal())) == 0)  {
//                    System.out.println("cannot be TRUE");
                    v2.removeVal(1,this,false);
                }
                else {
//                    System.out.println("cannot be FALSE");
                    v2.removeVal(0,this,false);
                }
            }
    }

    /**
     * Checks if the constraint is satisfied when the variables are instantiated.
     */
    @Override
    public boolean isSatisfied(int[] tuple) {
//        System.out.println("# IsSatisfied? ("+v0.getName()+" - "+v1.getName()+") - called");
        if (tuple[0] == 0) return true;
//      else if (tuple[1] < 0) /* trash value, not a reference */ return false;
        else if (dm.get(tuple[1]) == null) /* trash value, not a reference */ return false;
        else return (tuple[2] ==  buffer.check(predicate,new ArrayList(),dm.get(tuple[1])));
    }


    public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
//        throw new RuntimeException("Lazy constraints can occur only in positive positions!");
//        DEBUG("### OPOSITE");
        return new NegDynPredicate(this);
    }

//    public AbstractSConstraint<IntDomainVar> opposite(Solver solver) {
////        throw new RuntimeException("Lazy constraints can occur only in positive positions!");
//        return new NegLazyPredSConstraint(v0,v1,v2,data,buffer,predicate,functions);
//    }

    public String pretty() {
        return predicate.toString()+ "("+getVar(0).getName()+")";
    }


    private class NegDynPredicate extends AbstractSConstraint<IntDomainVar> {

        final DynPredicate hc;

        public NegDynPredicate(DynPredicate hc) {
            super(hc.vars);
            this.hc = hc;
//            DEBUG("### CREATING NEGATION");
        }

        public void propagate() throws ContradictionException {
            throw new RuntimeException("negation of a DynPredicate should NOT be propagated!");
        }

        @Override
        public boolean isConsistent() {
            throw new RuntimeException("negation of a DynPredicate should NOT be checked for isConsistent!");
        }

        public boolean isSatisfied() {
            throw new RuntimeException("negation of a DynPredicate should NOT be checked for isSatisfied!");
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