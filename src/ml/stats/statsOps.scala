package ml.stats

object statsOps {
  //get mean of a Vector
  def mean[T](v: Iterable[T])( implicit num: Numeric[T] ): Double = {
    num.toDouble(v.sum) / v.size;
  }
  
  // get sum of a Vector
  def sum[T](v: Iterable[T])(implicit num: Numeric[T]): Double = {
    num.toDouble(v.sum);
  }
  
  // get product of a Vector
  def prod[T](v: Iterable[T])(implicit num: Numeric[T]): Double = {
    num.toDouble(v.product);
  }
  /**
   * Generates a Set of Random numbers between the min and max found in an Iterable
   * 
   * @constructor create a random set with numbers between min and max
   * @param data vector containing data
   * @param size of set to be returned
   */
  def randomWithin[T: Numeric: ClassManifest](data: Iterable[T], size: Int): Array[Double] = {
    // Create a Set of many random numbers between min and max
    val rand = scala.util.Random
    val max = data.max.asInstanceOf[Double];
    val min = data.min.asInstanceOf[Double];
    
    var samples = Set[Double]()
    
    while(samples.size < size){
      samples = samples + (min + rand.nextDouble() * (max - min));
    }
    
    samples.toArray;
  }
  
}