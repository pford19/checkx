package pford19.checkx

/** Methods that compute standard factorial, permutation and combination counts.
  * <p>
  * <h4>{@code factorial}</h4>
  * {@code factorial(n) == n!, n*(n-1)*(n-2) ... *2*1}.  The value is a Double because it gets quite large
  *   quickly. For n > 170, the result is Double.Infinity.
  * <p>
  * <h4>{@code permutationCount}</h4>
  * <
  * {@code permutationCount(m,n)} is the number of distinct ordered selections of size {@code m} from of a set of size {@code n}.
  * For example, {@code permutationCount(2,4) == 4*3 == 12}.
  * <p>
  * <h4>{@code combinationCount}</h4>
  * {@code combinationCount(m, n)} is the number of distinct unordered subsets of size {@code m} from a set of size {@code n}.
  * For example {@code combinationCount(2, 4) == 4*3/2 == 6}.
  */

object Combinatorics {

  /** `n!`, the number of permutations of a set of size `n`.  The result is a Double
    * because it gets big fast.  171! == Double.Infinity
    *
    * @param n arbitrary Int >= 0
    * @return `n` factorial, Double.Infinity for n >= 171.
    */

  def factorial(n: Int): Double = permutationCount(n, n)

  /** n! if fits in a Long, otherwise -1.
    * Here is the list of factorials for n = 0 to 20. Any other
    * {{{
    * (0,1)
    * (1,1)
    * (2,2)
    * (3,6)
    * (4,24)
    * (5,120)
    * (6,720)
    * (7,5040)
    * (8,40320)
    * (9,362880)
    * (10,3628800)
    * (11,39916800)
    * (12,479001600)
    * (13,6227020800)
    * (14,87178291200)
    * (15,1307674368000)
    * (16,20922789888000)
    * (17,355687428096000)
    * (18,6402373705728000)
    * (19,121645100408832000)
    * (20,2432902008176640000)
    * }}}
    *
    * @param n non-negative. If <= 20, the factorial, otherwise -1.
    * @return n! or -1 if n > 20
    */
  def longFactorial(n: Int): Option[Long] = factorialTable.get(n)

  val factorialTable = Map[Int, Long](
    (0, 1L)
    , (1, 1L)
    , (2, 2L)
    , (3, 6L)
    , (4, 24L)
    , (5, 120L)
    , (6, 720L)
    , (7, 5040L)
    , (8, 40320L)
    , (9, 362880L)
    , (10, 3628800L)
    , (11, 39916800L)
    , (12, 479001600L)
    , (13, 6227020800L)
    , (14, 87178291200L)
    , (15, 1307674368000L)
    , (16, 20922789888000L)
    , (17, 355687428096000L)
    , (18, 6402373705728000L)
    , (19, 121645100408832000L)
    , (20, 2432902008176640000L)
  )

  /** Number of distinct permutations of `m` element subsets of a set of size `n`.
    *
    * @param m arbitrary non-negative integer, 0 <= m <= n
    * @param n arbitrary non-negative integer
    * @return n!/(n-m)!
    */
  def permutationCount(m: Int, n: Int): Double = {
    require(0 <= m)
    require(m <= n)
    val result = m match {
      case 0 => 1;
      case _ => (n - m + 1 to n).map(_.toDouble).product
    }
    result
  }

  /** n choose m. Number of distinct subsets of size m of a set of size n.
    *
    * @param m arbitrary non-negative integer, 0 <= m <= n
    * @param n arbitrary non-negative integer
    * @return n!/(m!)(n-m!)
    */
  def combinationCount(m: Int, n: Int): Double = {
    require(0 <= m)
    require(m <= n)
    if (m > n / 2)
      combinationCount(n - m, n)
    else {
      val p = (n - m + 1 to n).zip(1 to m)
      assert(p.size == m)
      val result = p.map { case (n, d) => n.toDouble / d.toDouble }.product
      result
    }
  }

}
