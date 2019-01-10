object session {
  println("hello world")
  1+4
  def abs(x: Double) = if (x<0) -x else x

  def sqrt(x: Double) = {
    def isGoodEnough(guess: Double, x: Double) =
      abs(guess * guess - x) < 0.001

    def improve(guess: Double, x: Double) =
      (guess + x / guess) / 2

    def sqrIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrIter(improve(guess, x), x)

    sqrIter(1.0, x)
  }

  sqrt(2)
  sqrt(4)
  //sqrt(1e60)
  def sqrt2(x: Double) = {

    def sqrtIter2(guess: Double, x: Double): Double =
      if (isGoodEnough2(guess, x)) guess
      else sqrtIter2(improve2(guess, x), x)

    def isGoodEnough2(guess: Double, x: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve2(guess: Double, x: Double) = (guess + x / guess) / 2

    sqrtIter2(1.0, x)
  }

  sqrt2(1e60)
}