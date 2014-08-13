package ml.util

object TypeConversions {
  /**
   * Convert from iterable of generic type T to vectorDouble
   */
  def TtoDouble[T](v: Iterable[T])(implicit num: Numeric[T]): Vector[Double] = {
    v.map(x => num.toDouble(x)).toVector
  }
}