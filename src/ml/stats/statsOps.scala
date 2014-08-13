package ml.stats

import ml.linalg.Table._

object statsOps {
  /**
   * Compute the mean of a vector
   * @param v a vector of any size
   * @return the mean of the vector
   */
  def mean[T: Numeric](v: Iterable[T]): Double = {
    sum(v) / v.size;
  }
  
  /**
   * Compute the median of a vector
   * @param v a vector of any size
   * @return the median of the vector
   */
  def median[T](v: Iterable[T])( implicit num: Numeric[T] ): Double = {
    val length = v.size;
    val sortedVector = v.map(x => num.toDouble(x)).toVector.sorted;
    
    val med: Double = if(length % 2 == 0){mean(Vector(sortedVector(length / 2),sortedVector((length / 2) -1) ));}
    	else{sortedVector(length / 2);}
    med
  }
  
  /**
   * Compute the mode of a vector
   * @param v a vector of any size
   * @return the mode of the vector
   */
  def mode[T: Numeric: ClassManifest](v: Iterable[T]): Vector[T] = {
    val t = table(v.toVector);
    val maxcount = t.counts.max
    
    // if maxcount is NOT greater than 1, then there is no mode
    if( maxcount > 1) t.data.filter(_._2 == maxcount).map(_._1);
    else Vector()
    
    
  }
  
  /**
   * Compute the range of a vector
   * @param v a vector of any size
   * @return the range of a vector
   */
  def range[T](v: Iterable[T])(implicit num: Numeric[T]): Double = {
    val max = v.map(x => num.toDouble(x)).toVector.max;
    val min = v.map(x => num.toDouble(x)).toVector.min;
    
    max - min;
    
  }
  
  /**
   * Compute the variance (sample or population) of a vector
   * 
   * @param v a vector of any size
   * @param kind either "sample" or "pop"
   * @return the variance of a vector
   */
  def variance[T](v: Iterable[T], kind: String = "sample")( implicit num: Numeric[T] ): Double = {
    val m = mean(v);
    
    val variance = kind.toUpperCase() match{
      case "SAMPLE" => (v.foldLeft(0.0){case(sum,(val1)) => sum + math.pow(num.toDouble(val1) - m, 2)}) / (v.size-1);
      case "POP" => (v.foldLeft(0.0){case(sum,(val1)) => sum + math.pow(num.toDouble(val1) - m, 2)}) / v.size;
      case _ => throw new IllegalArgumentException(kind +" is not a valid kind of variance");
    }
    variance;
  }
  
  /**
   * Compute the standard deviation (sample or population) of a vector
   * 
   * @param v a vector of any size
   * @param kind either "sample" or "pop"
   * @return the variance of a vector
   */
  def sd[T](v: Iterable[T], kind: String = "sample")( implicit num: Numeric[T] ): Double = {
    math.sqrt(variance(v, kind));
  }
  
  /*
   * Compute the standard error
   * 
   */
  def stdError[T](v: Iterable[T])(implicit num: Numeric[T]): Double = {
    sd(v) / math.sqrt(v.size);
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
   * @param data vector containing data
   * @param size of set to be returned
   */
  def randomWithin[T](data: Iterable[T], size: Int)(implicit num: Numeric[T]): Vector[Double] = {
    // Create a Set of many random numbers between min and max
    val rand = scala.util.Random
    val max = data.map(x => num.toDouble(x)).toVector.max;
    val min = data.map(x => num.toDouble(x)).toVector.min;
    
    var samples = Set[Double]()
    
    while(samples.size < size){
      samples = samples + (min + rand.nextDouble() * (max - min));
    }
    
    samples.toVector;
  }
  
  /**
   * Select n random vectors from a Matrix
   */
  def randomInts[T](min: T, max: T, n: Int)(implicit num: Numeric[T]): Vector[Int] = {
    val rand = scala.util.Random
    val small = num.toInt(min);
    val large = num.toInt(max);
    
    var samples = Set[Int]()
    
    while(samples.size < n){
      samples = samples + (small + rand.nextInt(large-small))
    }
    
    samples.toVector
  }
  
}