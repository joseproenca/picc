package picc.connectors.constraints.choco;

import choco.cp.model.managers.IntConstraintManager;
import choco.cp.solver.CPSolver;
import choco.kernel.model.constraints.ComponentConstraint;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.SConstraint;
import picc.connectors.constraints.Buffer;
import picc.connectors.constraints.Function;
import picc.connectors.constraints.Predicate;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * <p/>
 * Created by jose on 05/04/13.
 */
public class DynFuncManager extends IntConstraintManager {
    public SConstraint makeConstraint(Solver solver,
                                      IntegerVariable[] variables,
                                      Object parameters,
                                      List<String> options) {
        if (solver instanceof CPSolver) {
//            System.out.println("creating new lazy predicate with data "+((ArrayList<Object>)parameters).get(0));
//            IntDomainVar[] allVars = new IntDomainVar[variables.length]; // variables.map(solver.getVar(_))
//            for (int i = 0; i < variables.length; i++) {
//                allVars[i] = solver.getVar(variables[i]);
//            }
            return new DynFunction(
                    solver.getVar(variables[0]),solver.getVar(variables[1]),solver.getVar(variables[2]),solver.getVar(variables[3]),
                    ((ArrayList<DataMap>) parameters).get(0),
                    ((ArrayList<Buffer>) parameters).get(1),
                    ((ArrayList<Function>) parameters).get(2)
            );
        }
        return null;
    }


    /// STATIC AUXILIARY ///

    // boolvar of v1             , datavar of v1, boolvar of v2               , datavar of v2,dm,b,function
    static Constraint genFunction(IntegerVariable xvar, IntegerVariable xdata, IntegerVariable yvar, IntegerVariable ydata,
                                  DataMap dm, Buffer buf, Function pred) {
//        System.out.println("Creating generic predicate - "+xpred.getName()+" - "+pred);
        ArrayList<Object> parameters = new ArrayList<Object>();
        parameters.add(0,dm);
        parameters.add(1,buf);
        parameters.add(2,pred);
//        System.out.println("### CREATING DYN-FUNCTION");
        return new ComponentConstraint(DynFuncManager.class, parameters, new IntegerVariable[]{xvar,xdata,yvar,ydata});
    }

}
