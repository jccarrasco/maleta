package ml.metrics

import ml.linalg.Matrix

object distance {
	/**
   * Compute distance between two Vectors with the specified method. Supported distances include 
   * Euclidean, Manhattan, Minkowski
   * @param v1 vector containing n elements
   * @param v2 vector containing n elements
   * @param p the power of minkowski distance
   */
  def dist[T](v1: Iterable[T], v2: Iterable[T], method: String = "euclidean", p: Int = 2)(implicit num: Numeric[T]): Double = {
    val d: Double = method match{
      case "euclidean" => euclideanDist[Double](v1.asInstanceOf[Vector[Double]],v2.asInstanceOf[Vector[Double]]);
      case "Euclidean" => euclideanDist(v1,v2);
      case "Manhattan" => manhattanDist(v1,v2);
      case "manhattan" => manhattanDist(v1,v2);
      case "minkowski" => minkowskiDist(v1,v2, p);
      case "Minkowski" => minkowskiDist(v1,v2, p);
    }
    d
  }
  
  // euclidean distance
  private def euclideanDist[T](v1: Iterable[T], v2: Iterable[T])(implicit num: Numeric[T]): Double = {
   
    val vector1 = v1.asInstanceOf[Iterable[Double]];
    val vector2 = v2.asInstanceOf[Iterable[Double]];
    
    math.sqrt(vector1.zip(vector2).foldLeft(0.0){case(sum,(val1,val2)) => sum + math.pow(val1 - val2, 2)})
  }
  
  // manhattan distance
  private def manhattanDist[T](v1: Iterable[T], v2: Iterable[T])(implicit num: Numeric[T]): Double = {
    val vector1 = v1.asInstanceOf[Iterable[Double]];
    val vector2 = v2.asInstanceOf[Iterable[Double]];
    
    (vector1.zip(vector2).foldLeft(0.0){case(sum,(val1,val2)) => sum + (val1 - val2)})
  }
  
  // minkowski distance
  private def minkowskiDist[T](v1: Iterable[T], v2: Iterable[T], p: Int)(implicit num: Numeric[T]): Double = {
   
    val vector1 = v1.asInstanceOf[Iterable[Double]];
    val vector2 = v2.asInstanceOf[Iterable[Double]];
    
    math.pow(vector1.zip(vector2).foldLeft(0.0){case(sum,(val1,val2)) => sum + math.pow(val1 - val2, p)}, (1.toDouble/p))
  }
  
  /**
   * Find centroid of a Matrix
   */
  def centroid[T](m: Matrix[T])(implicit num: Numeric[T]): Vector[Double] = {
	  m.colMeans; 
  }
}