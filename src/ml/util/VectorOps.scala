package ml.util

object VectorOps {
  
  /**
   * Find indices that match a given n inside a vector
   * @param v vector of any size
   * @param n element to search inside a vector
   */
  def findIndices[T](v: Iterable[T], n: Int)(implicit num: Numeric[T]): Vector[Int] = {
    
    Range(0,v.size).zip(v).filter(_._2 == n).unzip._1.toVector
  }

}