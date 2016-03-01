package picc.connectors.constraints.choco;

import choco.cp.model.managers.IntConstraintManager;
import choco.cp.solver.CPSolver;
import choco.kernel.model.constraints.ComponentConstraint;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.SConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;
import picc.connectors.constraints.Buffer;
import picc.connectors.constraints.Function;
import scala.collection.immutable.List;

import java.util.ArrayList;

/**
 * Created with IntelliJ IDEA.
 * <p/>
 * Created by jose on 10/04/13.
 */
public class DynNFuncManager  extends IntConstraintManager {
    public SConstraint makeConstraint(Solver solver,
                                      IntegerVariable[] variables,
                                      Object parameters,
                                      java.util.List<String> options) {
        if (solver instanceof CPSolver) {
//            System.out.println("creating new lazy predicate with data "+((ArrayList<Object>)parameters).get(0));
            IntDomainVar[] allVars = new IntDomainVar[variables.length]; // variables.map(solver.getVar(_))
            for (int i = 0; i < variables.length; i++) {
                allVars[i] = solver.getVar(variables[i]);
            }
            return new DynNFunction(
                    allVars,
                    ((ArrayList<DataMap>) parameters).get(0),
                    ((ArrayList<Buffer>) parameters).get(1),
                    ((ArrayList<Function>) parameters).get(2)
            );
        }
        return null;
    }


    /// STATIC AUXILIARY ///

    // boolvar of v1             , datavar of v1, boolvar of v2               , datavar of v2,dm,b,function
    static Constraint genNFunction(IntegerVariable xvar, IntegerVariable xdata, List<IntegerVariable> yvar, List<IntegerVariable> ydata,
                                  DataMap dm, Buffer buf, Function pred) {
//        System.out.println("Creating generic predicate - "+xpred.getName()+" - "+pred);
        ArrayList<Object> parameters = new ArrayList<Object>();
        parameters.add(0,dm);
        parameters.add(1,buf);
        parameters.add(2,pred);
        IntegerVariable[] vars = new IntegerVariable[2+(yvar.size()*2)];
        vars[0] = xvar;
        vars[1] = xdata;
        for (int i=0; i<yvar.size(); i++) {
            vars[(i+1)*2]   = yvar.apply(i);  // 2, 4, 6...
            vars[(i+1)*2+1] = ydata.apply(i); // 3, 5, 7...
        }
//        System.out.println("### CREATING DYN-N-FUNCTION");
        return new ComponentConstraint(DynNFuncManager.class, parameters, vars);
    }

}
