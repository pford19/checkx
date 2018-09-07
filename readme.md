# checkx

Exploration of and small extensions to `ScalaCheck`.

## Summary

- Project: checkx
- Summary: `ScalaCheck` eXtensions and eXploration
- Author: Paul Ford, HERE Technologies
- Started: Aug 2018
- package: `org.here.cme.poc.checkx`

## Overview
 
The code in this project explores `ScalaCheck` properties.

Much of the exploration is in the form of functions on Generators, Shrink, Seed with corresponding
`ScalaCheck` property-based tests of these functions.

The impetus for this work was the need to develop some quick tests of a class
that operated on what was effectively a tree. The idea of property based testing has 
always appealed to me, so this seemed like a great opportunity to dig into `ScalaCheck` a bit more 
deeply. 

It turned into an interesting rabbit hole. How do generators work? How does Seeds work? How does Shrink work? 
Why does exists give up? What's with all the parameters? Which ones are relevant? How are they set?
## Topics Explored
Here are some of the topics investigated

 - deterministic generators [open]
 - discard logic, e.g., with exists() [open]
 - property combinators: all() [done]
 - collect [done]
 - how `extend Properties` works [done]
 - `Shrink` [getting there]
 - `Seed` [getting there]
 - Generator parameters, such as size [open]
 - Test parameters [in progress]
 - How does `sieve` work on Generator and Result? Especially w.r.t. `Shrink` [open]
 - How does `seed0` thread through the stream of `Gen` results? [open]
 
## Extensions
 
### Classes

`checkx` includes two type families

- Permutations
- Partitions

#### Permutations

Support for finite permutation groups (the mathemtatical kind). 

Wikipedia references for [Permutation Groups](https://en.wikipedia.org/wiki/Permutation_group), and [Permutations](https://en.wikipedia.org/wiki/Permutation#Cycle_notation)

It has three classes
- `trait Perm`
- `case class Pmap(rep: Map[Int,Int])`
- `object Identity extends Perm`
- `class Transposition(rep: (Int, Int)) extends Perm`
- `class Cycle(rep: Seq[Int]) extends Perm`
- `class Permutation(rep: Seq[Int]) extends Perm`
- `class PermGroup(n: Int)`

`Perm` is a trait common to the above types of permutation. Some common implementations are provided. In particular 
`toString`, `final equals` and `final hashCode`

`Pmap` provides a common minimal representation for any permutation. It encapsulates a `Map[Int,Int]` 
that is (1) an *automorphism* and (2) has no fixed points. A `Map` is 
an automorphism if its key set and value set are identical. Having no fixed points makes the map minimal in size. 

`Identity` is a static object representing the permutation that doesn't permute anything.

`Transposition` instances are cycles of just 2 elements. Transpositions are the building block for all other permutations.

`Cycle`instances are permutations whose action is cyclic. That is, the non-fixed points form a single orbit

`Permutation` instances are individual general permutations. `Permutation` instances because they are general can represent 
the same permutation as `Identity`, or a `Transposition`, or `Cycle`. Because of the shared `equals` a `Permutation` instance
will compare as equal (`==`) to an equivalent `Cycle` or `Transposition` instance.

`PermGroup` represents the full permutation group on n-elements, also known as the Symmetric group.
It can produce selected elements and subsets of the full permutation group. 
    
`PermutationIterator` 
iterates over all `n!` (factorial) permutations of degree `n`. Recall that `n!` grows
very large very fast. 
For example, `PermGroup(10)` has `10! = 3,628,800` elements. `20! = 2,432,902,008,176,640,000` (over 2 quintillion), 
while <code>100! = 9.33 * 10<sup>157</sup></code>.

The implementation is a true iterator and generates only one value at a time. It only needs to preserve a small amount of 
prior state to generate the next value. So, for example, it is possible to iterator over the 3 million plus permutations
of degree 10 without having to allocate storage for more than one at a time. This, combined with `ScalaCheck` style generators,
supports certain styles of exhaustive testing (at least for small N).


### Generators

`checkx` include some new generators 
 - `orderedPick` \[done but not deterministic]
 - `permutations` \[done but not deterministic]
 - `partitions`

## TODO

- Make `orderedPick` deterministic
- More property tests.
- More docs.
