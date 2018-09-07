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
- `case lass PermGroup(n: Int)`
- `case class Cycle(rep: List[Int])`
- `case class Permutation(rep: List[Int])`

`PermGroup` represents the full permutation group on n-elements, also known as the Symmetric group.
It can produce selected elements and subsets of the full permutation group. 
    
`PermutationIterator` 
iterates over all `n!` (factorial) permutations of degree `n`. Recall that `n!` grows
very large very fast. 
For example, `PermGroup(10)` has `10! = 3,628,800` elements. `20! = 2,432,902,008,176,640,000` (over 2 quintillion), 
while <code>100! = 9.33 * 10<sup>157</sup></code>.

The implementation is a true iterator and generates only one value at a time. It only needs to preserve a small amount of 
prior state to generate the next value. So, for example, it is possible to iterator over the 3 million plus permutations
of degree 10 without having to allocate storage for all of them. 

`Permutation` instances are individual general permutations. 

`Cycle` instances are special permutations whose action is cyclic, the non-fixed points form a single orbit

`Transposition` instances are cycles of just 2 elements

`Identity` is a static object representing the permutation that doesn't permute anything.

`Perm` is a trait common to the above types of permutation.

`Pmap` provides a common minimal representation for any permutation. It encapsulates a `Map[Int,Int]` that is (1) an *automorphism* and (2) has no fixed points. A `Map` is 
an automorphism if its key set and value set are identical. Having no fixed points makes the map minimal in size. 


### Generators

`checkx` include some new generators 
 - `orderedPick` \[done but not deterministic]
 - `permutations` \[done but not deterministic]
 - `partitions`

## TODO

- Make `orderedPick` deterministic
- More property tests.
- More docs.
