# S-99: Ninety-Nine Scala Problems

This project is my attempt at solving the [99 Scala problems][1]. The
specifications were built by [etorreborre][6] as a set of [specs2][2]
specifications, ready to implement and execute with [sbt][3], and further
improved by [ruippeixotog][7].

Each Specification is divided into:

* the description of the problem to solve (see [`ListsSpec`][4]) for an
  example)
* the methods to implement/solutions (see [`ListsSolutions`][5] for an example)

To run the specifications, install sbt (version > 0.11.2) and execute:

    > test

To see only the failed ones:

    > test-only -- xonly

*WARNING*: The specifications have not yet been all tested against a valid
 implementation, please report any bug!

[1]: http://aperiodic.net/phil/scala/s-99/
[2]: http://specs2.org
[3]: https://github.com/harrah/xsbt/
[4]: https://github.com/etorreborre/s99/blob/master/src/test/scala/s99/ListsSpec.scala
[5]: https://github.com/etorreborre/s99/blob/master/src/main/scala/s99/ListsSolutions.scala
[6]: https://github.com/etorreborre/s99
[7]: https://github.com/ruippeixotog/s99
