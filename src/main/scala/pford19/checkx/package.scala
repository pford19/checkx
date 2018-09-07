package pford19

/** This packages consists of
  *   - eXperiments in scalacheck
  *   - eXtensions to scalacheck
  *   - supporting classes
  *
  *  Hence the name `checkx`.
  *
  *  Many of the experiments are in the form units tests (both `ScalaCheck` and `ScalaTest`.
  *
  *  A couple of stackoverflow examples are worked out.
  *
  *  The extensions are found in [[pford19.checkx.SomeNewGenerators SomeNewGenerators]], [[pford19.checkx.SeedOps SeedOps]],
  *  and [[pford19.checkx.PermutationGenerator]]
  *  Some of these are probably overkill and as I better understand scalacheck I can do away with or simplify.
  *
  *  The extensions include
  *   - compute a (random) Int` from a [[org.scalacheck.rng.Seed]]
  *   - orderedPick: a generator like pick but that preserves order of the source sequence
  *   - partitions: generator for partitions of an integer
  *   - permutations: permutation generators
  *   - methods to convert between generators, iterators, and streams
  *   - random number generators
  *   - Seed generators
  *   - group (or tuple) generator
  *
  *
  *  Supporting classes include
  *   - combinatoric functions for counting combinations
  *   - permutations and an exhaustive iterator over permutations of a specified degree
  *   - a `Stats` object that computes a few simple statistics from a vector of numeric data
  * */
package object checkx {

}
