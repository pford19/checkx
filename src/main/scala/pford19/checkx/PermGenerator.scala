package pford19.checkx

import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import Combinatorics._


object PermGenerator {

  val DEBUGGING_ON = false
  val DEBUG_LOOP_INTERNAL_ON = false

  /** State vector, persisted across successive calls to next().
    *
    * @param inner local (inner) state vector
    * @param outer exposed (outer) state vector
    */
  case class CombinedState(inner: InnerState, outer: IterationState) {
    require(outer.degree >= 2) // degree 0 and 1 are trivial since Sym(0) = Sym(1) = I.

    def dumpState(msg: String): String =
      s"""$msg
         | ${this.outer.dumpState("Outer State")}
         | ${this.inner.dumpState("Inner State")}
        """.stripMargin


    //    def applyOuterState(os: OuterState): CombinedState = this.copy(outer = os)

    //def applyInnerState(is: InnerState): CombinedState = this.copy(inner = is)

  }

  /**
    * @param degree    degree of the permutation
    * @param p         accumulated counters indexed by n
    * @param d         1 or -1 indexed by n,  used to increment/decrement corresponding p slot
    * @param perm      current permutation
    * @param callCount number of calls
    */
  case class IterationState(degree: Int,
                            p: mutable.ArraySeq[Int],
                            d: mutable.ArraySeq[Int],
                            perm: Permutation,
                            lastTransposition: Cycle,
                            callCount: Int) {

    require(degree >= 0, dumpState("require degree >= 0"))
    require(p.size == degree + 1, dumpState("require p.size==degree+1)"))
    require(p.size == d.size, dumpState("require p.size==d.size)"))
    require(perm.degree == degree, dumpState(s"require perm.degree == degree, but perm.degree=${perm.degree}"))

    def dumpState(msg: String): String =
      s"""$msg
         | degree=${this.degree}, call count=${this.callCount}
         | p=${this.p.tail}, d=${this.d.tail}, lastTrans=${this.lastTransposition} perm=${this.perm.cyclicRepresentation}
        """.stripMargin

  }

  /**
    * @param n                     index
    * @param q                     index
    * @param k                     index offset
    * @param goto_final_exit       control flag, signals that iteration is complete and that next entry will reinitialize
    * @param goto_transpose        loop control flag, signals an exit from the loop
    * @param reinitialize_on_entry control flag, signals need to reinitialize state on entry
    *
    */
  case class InnerState(
                         n: Int,
                         k: Int,
                         q: Int,
                         goto_final_exit: Boolean,
                         goto_transpose: Boolean,
                         reinitialize_on_entry: Boolean
                       ) {
    require(!(goto_final_exit && goto_transpose), "state invariant: both goto's can't be true")

    def dumpState(msg: String): String =
      s"""$msg
         | n=${this.n}, q=${this.q}, k=${this.k}
         | final_exit=${this.goto_final_exit}, goto_transpose=${this.goto_transpose}, first=${this.reinitialize_on_entry}
        """.stripMargin

  }

  object CombinedState {
    private val UNUSED_SLOT: Int = -99

    /**
      * Initialize a PG_State object from permutation `s`.
      *
      * @param degree degree of permutations to generate
      * @return new state
      */
    def initialState(degree: Int): CombinedState = {

      new CombinedState(
        inner = StateOps.initialLocalState(degree),
        outer = StateOps.initialIterationState(degree)
      )
    }

  }

  object StateOps {

    def initialLocalState(degree: Int): InnerState = {

      new InnerState(
        n = degree,
        q = 0,
        k = 0,
        goto_final_exit = false,
        goto_transpose = false,
        reinitialize_on_entry = false,
      )

    }

    def initialIterationState(degree: Int): IterationState = {
      // These vectors are copied from the original algorithm.
      // They are declared as Algol arrays with index range[2:Degree].
      // Scala equivalent is an array of size (degree+1) with slots 0 and 1 unused.
      // Slot 0 is unused because the algol algorithm indexes the permuted set from 1.
      // Slot 1 is unused because the algol algorithm only requires degree-1 slots indexed from 2 (if 2 >= degree)
      // Active slots are indexed from 2 to degree. For degree 0 or 1 there are no active slots.
      // For trival cases degree = 0 or 1 (since S(0) = S(1) = I) these arrays have size 1 and 2 respectively.
      val newd = mutable.ArraySeq.fill(degree + 1)(1)
      val newp = mutable.ArraySeq.fill(degree + 1)(0)

      val result = new IterationState(degree = degree,
        p = newp,
        d = newd,
        perm = PermGroup(degree).identity,
        lastTransposition = Cycle(0),
        callCount = 0)
      result
    }
  }

  /**
    * Unbounded and cyclic iterator over `IterationState`.  The cycle length is `degree!`.
    * This state encapsulates a current permutation and the "difference" transposition as well as a call count.
    * <p>
    * <p>
    * The cycle length is `degree!`. The last state in each cycle encapsulates the identity transformation. Namely
    * {{{
    *   val factorial = degree!
    *   val it = stateIterator(degree)
    *   (1 to 10*factorial).foreach {
    *     val s = it.next
    *     val p = s.perm
    *     val j = s.callCount
    *     assert((j%factorial) != 0 || p.isIdentity)
    * }}}
    * <p>
    *
    * @param degree permutation degree, size of permuted set, must be >= 2
    * @return Iterator over
    */
  def unboundedStateIterator(degree: Int): Iterator[IterationState] = {
    var nextstate = nextPermutation(StateOps.initialIterationState(degree))
    new Iterator[IterationState] {
      val hasNext: Boolean = true

      def next(): IterationState = {
        val result = nextstate
        nextstate = nextPermutation(nextstate)
        result
      }
    }
  }

  /** Finite iterator over all permutations of of degree `degree`. The [[IterationState]] values
    * returned by the iterator encapsulate the successive permutations (`IterationState.perm`) as well
    * as the last transposition (`IterationState.lastTransposition`).
    * <p>
    * The final iterator value encapsulates the identity transformation.
    *
    * @param degree permutation degree, the size of the set acted on by the permutations, must be >= 2
    * @return Iterator over all permutations of the given degree
    */
  def stateIterator(degree: Int): Iterator[IterationState] = {
    unboundedStateIterator(degree).span(!_.perm.isIdentity) match {
      case (i1, i2) => i1 ++ i2.take(1)
    }
  }

  /** Finite iterator over degree! distinct permutations of degree `degree`.
    *
    * @param degree permudation degree, size of set acted on, >= 0
    * @return iterator over all permutations of given degree, identity is last
    */

  def permutationIterator(degree: Int): Iterator[Permutation] = degree match {
    case 0 => List(PermGroup(0).identity).toIterator
    case 1 => List(PermGroup(1).identity).toIterator
    case _ => stateIterator(degree).map(_.perm)
  }

  /** Finite iterator over `degree!-1` transpositions (2-cycle) that are the "differences" between
    * successive permutations returned by `permutationIterator`.
    * <p>
    * The following pseudo code captures this semantic
    * {{{
    *   val ps: Seq[Permutation] = permIterator(degree).toSeq
    *   val ts: Seq[Cycle] = transpositionIterator(degree).toSeq
    *   assert(ps.size = ts.size + 1)
    *   (0 to ps.size-1).foreach {
    *     assert(ts(i).size == 2) // transposition
    *     assert(ps(i) * ts(i) == ps(i+1) // ts(i) is the "difference", ps(i+1)*ps(i).inverse
    *   }
    * }}}
    *
    * @param degree permudation degree, size of set acted on, >= 0
    * @return iterator over all permutations of given degree, identity is last
    */

  def transpositionIterator(degree: Int): Iterator[Cycle] = degree match {
    case 0 => Iterator.empty
    case 1 => Iterator.empty
    case _ => stateIterator(degree).drop(1).map(_.lastTransposition)
  }


  /** The next permutation based on the input state. Return the result and the next state. The next state is suitable
    * as input to generate the next value in the stream.
    * <p>
    * Valid input values for IterationState are either (1) CombinedState.initialOuterState or (2) a value returned by
    * a call to nextPermutation.
    * <p>
    * Appropriately wrapped, this function is the basis for an iterative function, an Iterator, or a Stream.
    * This package provides those standard wrappers.
    * <p>
    * Additional wrappers with more sophisticated behavior can be customized as well.
    * IterationState includes both a permuation and a generator transposition.
    *
    * @param state0 valid seed state, with degree >= 2
    * @return pair of (next permutation, next state)
    */
  def nextPermutation(state0: IterationState): IterationState = {
    state0.degree match {
      case 0 => state0.copy(callCount = state0.callCount + 1)
      case 1 => state0.copy(callCount = state0.callCount + 1)
      case _ => nextPermutation2(state0)
    }

  }

  def nextPermutation2(state0: IterationState): IterationState = {


    var iterationState = state0
    // Copy p and d to allow them to be mutated in place locally.
    // These ArraySeq are returned in the result state.
    val p: mutable.ArraySeq[Int] = state0.p.clone()
    val d: mutable.ArraySeq[Int] = state0.d.clone()
    var localState = StateOps.initialLocalState(state0.degree)

    def mutateState(f: => Any) = {
      f // evalute f for side effects on localState and iterationState
    }

    var loopCount = 0

    // LOGIC

    mutateState {
      // init_p_and_d
      assert(!localState.reinitialize_on_entry, localState.dumpState(("expect reinit on entry false")))
      localState = localState.copy(n = state0.degree, k = 0, goto_final_exit = false, goto_transpose = false, reinitialize_on_entry = false)
      iterationState = iterationState.copy(callCount = iterationState.callCount + 1)
    }

    def logLoopState(tag: String = "", loopCount: Int): Unit = {
      if (DEBUG_LOOP_INTERNAL_ON) {
        val s = CombinedState(localState, iterationState)
        println(f"LOOP INTERNAL: $tag%20s call=${s.outer.callCount}%04d complete=$loopCount deg=${s.outer.degree}%02d " +
          f"n=${s.inner.n}%d p(${s.inner.n}%d)=${s.outer.p(s.inner.n)}%+2d d(${s.inner.n}%d)=${s.outer.d(s.inner.n)}%+2d q=${s.inner.q}%02d k=${s.inner.k}%02d")
      }

    }

    assert(localState.k == 0 && localState.n == state0.degree && !localState.goto_transpose && !localState.goto_final_exit && !localState.reinitialize_on_entry, localState.dumpState("failed assertion on entry"))

    loopCount = 0
    var loopStartIteration = iterationState.copy()
    val loopStartLocal = localState.copy()
    val MAX_LOOP_COUNT = state0.degree - 1
    logLoopState("loop start", loopCount)
    do {

      // update p and q
      mutateState {

        assert(1 < localState.n && localState.n <= iterationState.degree, localState.dumpState("1 < n <= degree"))
        val newq = p(localState.n) + d(localState.n)
        p(localState.n) = newq
        assert(newq <= localState.n, localState.dumpState(s"newq==$newq, !<=n"))
        localState = localState.copy(q = newq)
        logLoopState("update p and q", loopCount)
      }

      if (localState.q == localState.n)
      //set d(n) = -1
        mutateState {
          d(localState.n) = -1
          logLoopState("set d(n)=-1", loopCount)
        }
      else if (localState.q != 0)
      // goto transpose
        mutateState {
          localState = localState.copy(goto_transpose = true)
          logLoopState(s"goto transpose", loopCount)
        }

      else
      // reset d(n) = 1 and k += 1
        mutateState {
          val newk = localState.k + 1
          d(localState.n) = 1
          localState = localState.copy(k = newk)
          logLoopState(s"d(n)=1, k+=1", loopCount)
        }


      //loop_finalize
      if (!localState.goto_transpose) {
        if (localState.n > 2)
          mutateState {
            localState = localState.copy(n = localState.n - 1)
            logLoopState(s"n=n-1", loopCount)
          }
        else
          mutateState {
            localState = localState.copy(goto_final_exit = true)
            logLoopState(s"goto final exit", loopCount)
          }
      }


      loopCount += 1
      assert(loopCount <= state0.degree - 1,
        s"loop count $loopCount exceeds max ${state0.degree - 1}" +
          s", local state=${localState.dumpState("end of loop body")}" +
          s", iteration state=${iterationState.dumpState("end of loop body")}"
      )
    }

    while (!localState.goto_transpose && !localState.goto_final_exit)
    logLoopState(s"loop exit", loopCount)

    // Comments on this loop
    // It executes anywhere from 1 to degree-1 times, depending on the number of calls.
    // Let c be the call number.
    // Let d2, d3, d4, ..., d(d-1) be the sequence of (2 to d-1).map(degree!/_!)
    // If c is a multiple of d2, then the loop count is degree-1.
    // Otherwise if c is a multiple of d3, then the loop count = degree-2.
    // Otherwise if c is a multiple of d4, then the loop count = degree-3.
    // Otherwise if c is a multiple of d5, then the loop count = degree-4.
    // Otherwise if c is a multiple of d6, then the loop count = degree-5.
    // And so on.
    //
    // Example: degree = 5. 5! = 120, d2=60, d3=20, d4=5, d5=1.
    // If c is a multiple of 60 = 5*4*3, then loops = 4.
    // Otherwise if c is a multiple of 20 = 5*4, then loops = 3.
    // Otherwise if c is a multiple of 5, then loops = 2.
    // Otherwise loops = 1.

    // On loop entry: goto_transpose and goto_final_exit are both false
    // On loop exit: exactly one is set to true. They are never both true. (Enforced as an invariant on combined state)
    // If call count is a multiple of degree! then goto_final_exit is true. Otherwise goto_transpose is true.
    //


    // Detailed logging at this point to help develop an understanding of loop behavior.
    if (DEBUGGING_ON) {
      val toggled_final = loopStartLocal.goto_final_exit != localState.goto_final_exit
      val toggled_transpose = loopStartLocal.goto_transpose != localState.goto_transpose
      val exclusive_toggle = toggled_final != toggled_transpose
      assert(exclusive_toggle, s"final or transpose, but not both ${localState.dumpState("loop exit")}")
      val ddiff = iterationState.d.tail.tail.sum - loopStartIteration.d.tail.tail.sum
      val ndiff = localState.n - loopStartLocal.n
      val kdiff = localState.k - loopStartLocal.k
      val d_mutated = if (loopStartIteration.d != iterationState.d) f"d${ddiff}%+2d" else "   "
      val k_mutated = if (loopStartLocal.k != localState.k) f"k${kdiff}%+2d" else "   "
      val n_mutated = if (loopStartLocal.n != localState.n) f"n${ndiff}%+2d" else "   "

      val exit_condition = if (toggled_final) " f" else "t " // f for goto_final_exit, t for goto_transpose

      val summary = List(exit_condition, d_mutated, n_mutated, k_mutated).mkString(" ")


      if (DEBUGGING_ON)
        println(f"DEBUG >>>> LOOP degree=${iterationState.degree}%3d, call=${iterationState.callCount}%4d, loop=$loopCount%2d, $summary")
      if (DEBUGGING_ON)
        println(List(
          loopStartIteration.dumpState("loop start"),
          loopStartLocal.dumpState("loop start"),
          iterationState.dumpState("loop end")),
          localState.dumpState("loop end")
            .mkString("\n"))
    }

    if (localState.goto_final_exit)
      mutateState {
        localState = localState.copy(q = 1, reinitialize_on_entry = true)
      }

    // Each execution of this function does a transposition on outerState.perm to compute the next permutation in the series. Here is where it happens ....
    mutateState {
      // transpose
      val t = localState.q + localState.k
      assert(t > 0, localState.dumpState("q > 0"))
      assert(t < iterationState.degree, localState.dumpState(s"q < degree=${iterationState.degree}"))
      val trans = Cycle(t - 1, t) // Cycles act on (0 to degree-1)
      val newperm = iterationState.perm * trans
      iterationState = iterationState.copy(
        perm = newperm,
        lastTransposition = trans,
        p = p, // not a clone, return a reference to the local ArraySeq
        d = d // ditto
      )
    }

    mutateState {
      if (localState.reinitialize_on_entry) {
        iterationState = StateOps.initialIterationState(state0.degree)
          .copy(callCount = iterationState.callCount, lastTransposition = iterationState.lastTransposition)
      }
    }

    // SOME POSTCONDITIONS


    // call count incremented by 1
    assert(iterationState.callCount == state0.callCount + 1)

    // permutation has been permuted
    // since degree >= 2, don't have to worry about trival cases of degree 0 and 1
    assert(iterationState.perm != state0.perm)

    // degree! is cycle length, perm should be the identity when call count is a multiple of the cycle length
    // degreeFactorial == None signals that degree! is too large to be represented as a Long
    // This postcondition reads:
    // (degree! can be represented as a Long) && (callCount is a multiple of degree!) IMPLIES iterationState.perm.isIdentity
    {
      /** degree! (factorial), if it is small enough to be represented as an exact Double integer value. (37 bits), otherwise
        * -1.  Used to assert postcondition on cycles for small degree.
        */
      val d = Combinatorics.longFactorial(state0.degree)
      if (d.nonEmpty)
        assert(iterationState.callCount % d.get != 0 || iterationState.perm.isIdentity)
    }

    iterationState
  }
}

object ALGOL_SOURCE {
  """
    |Communications of the AMC Volume 5, Issue 8, Aug 1962, p440
    |Algorithm 115, PERM,
    |H. F. Trotter, Princeton University.
    |
    |boolean first = true
    |
    |procedure PERM (x, n); value n;
    |integer n; array x;
    |
    |begin
    |
    |  own integer array p, d[2:n];
    |  integer k, q;
    |  real t;
    |
    |  if first then initialize:
    |  begin
    |    for k := 2 step 1 until n do
    |    begin
    |      p[k] := 0; d[k] := 1
    |    end;
    |    first := false
    |  end initialize;
    |
    |  k:=0;
    |
    |  INDEX:
    |  p[n] := q := pin] + d[n];
    |  if q = n then
    |    begin din] := -1; go to LOOP end;
    |  if q # 0 then
    |    go to TRANSPOSE;
    |  d[n] := 1;
    |  k := k + 1;
    |
    |  LOOP:
    |  if n > 2 then
    |  begin
    |    n:=n- 1;
    |    goto INDEX
    |  end LOOP;
    |
    |  Final exit:
    |  q := 1;
    |  first := true;
    |
    |  TRANSPOSE:
    |  q := q + /c;
    |  t := x[q];
    |  x[q]:=x[q+1];
    |  x[q+1]:=t
    |
    |end PERM ;
    |
  """.stripMargin
}
