PICC: Partial Interactive Coordination Constraints
============================================

This project encodes in [Scala](http://www.scala-lang.org) a constraint-based engine for running connectors based on [Reo](https://en.wikipedia.org/wiki/Reo_Coordination_Language).

This is a simplified version adapted from [another project](https://github.com/joseproenca/ip-constraints), removing unnecessary generality and experiments with different constraint solvers and semantics.
It is still under construction - very few examples, and no partiality included yet.

The coordination engine operates in rounds, each of which proceeds by collecting constraints from components and the connector that coordinates them, and then solving the constraints. Components perform blocking reads and writes on ports, which are converted into constraints stating that they want to output or input data. A solution to the constraints describes how data flows between the components, after which some reads and writes may succeed. Each round is considered to be an atomic (or synchronous) step. Between rounds the states of the components and connectors may change.

In practice, connectors are specified as the composition of simpler primitive connectors, and each of these primitive connectors specifies:

  * its coordination constraints; and
  
  * how to update its state based on the solution for the last round.

To give a flavour of how to define primitive connectors, a few examples of primitive connectors can be found in [Connectors](code/src/reopp/common/guardedcommands/dataconnectors).

Examples of some more complex connectors built using these primitive ones can be found in [Examples](code/src/reopp/common/examples).


Compile and test with SBT
-----------------

Use sbt (+0.13) to compile:
```
sbt compile
```
The unit tests can be similarly ran with sbt:
```
sbt test
```


Try it out
----------
To experiment, the easiest way is to create a console:
```
sbt console
```

You can then import the DSL and start creating and executing connectors, as exemplified below
```scala
// import the DSL constructors
scala> import picc.DSL._
// define connector with a writer with 3 values, a fifo buffer, and a reader that accepts 2 values
scala> def c = writer("a",List(5,6,7)) ++ fifo("a","b") ++ reader("b",2)
//executes the connector until no behaviour is possible
//(until the reader cannot receive more data)
scala> c.run()
```

Here only ```writer```, ```fifo```, and ```reader``` were used, but many other Reo primitive exist. See more examples here and here to have a better idea of other primitives that can be used.
