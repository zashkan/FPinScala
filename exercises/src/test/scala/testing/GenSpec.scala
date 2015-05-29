/*
Ex. 8.1

Some properties (expressed as functions) to test an implementation of
sum: List[Int] => Int

  1. xs: List[Int] => sum(xs) == sum(xs.reverse)

  2. n: Int => x: Int => sum(List.fill(n)(x)) == n * x

  3. sum(List.empty) == 0

  4. x: Int => sum(List(x)) == x

Ex. 8.2

Properties that specify an implementation of maximum: List[Int] => Int

  1. x: Int => maximum(List(x)) == x

  2. n: Int => x: Int => maximum(List.fill(n)(x)) == x

  3. xs: List[Int] => maximum(xs) == maximum(xs.reverse)
*/

