package ml.metrics

import ml.linalg.Matrix
import ml.util.TypeConversions._

import scala.collection.mutable.ArrayBuffer

object distance {
	/**
   * Compute distance between two Vectors with the specified method. Supported distances include 
   * Euclidean, Manhattan, Minkowski
   * @param v1 vector containing n elements
   * @param v2 vector containing n elements
   * @param p the power of minkowski distance
   */
  def dist[T: Numeric: ClassManifest](v1: Iterable[T], v2: Iterable[T], method: String = "euclidean", p: Int = 2): Double = {
    val d: Double = method.toUpperCase() match{
      case "EUCLIDEAN" => euclideanDist(v1,v2);
      case "MANHATTAN" => manhattanDist(v1,v2);
      case "MINKOWSKI" => minkowskiDist(v1,v2, p);
      case _ => throw new IllegalArgumentException(method.toUpperCase() + "is not supported!")
    }
    d
  }
  
  // euclidean distance
  private def euclideanDist[T](v1: Iterable[T], v2: Iterable[T])(implicit num: Numeric[T]): Double = {
   
    val vector1 = v1.map(x => num.toDouble(x)).toVector;
    val vector2 = v2.map(x => num.toDouble(x)).toVector;
    
    math.sqrt(vector1.zip(vector2).foldLeft(0.0){case(sum,(val1,val2)) => sum + math.pow(val1 - val2, 2)})
  }
  
  // manhattan distance
  private def manhattanDist[T](v1: Iterable[T], v2: Iterable[T])(implicit num: Numeric[T]): Double = {
    val vector1 = v1.map(x => num.toDouble(x)).toVector;
    val vector2 = v2.map(x => num.toDouble(x)).toVector;
    
    (vector1.zip(vector2).foldLeft(0.0){case(sum,(val1,val2)) => sum + math.abs(val1 - val2)})
  }
  
  // minkowski distance
  private def minkowskiDist[T](v1: Iterable[T], v2: Iterable[T], p: Int)(implicit num: Numeric[T]): Double = {
   
    val vector1 = v1.map(x => num.toDouble(x)).toVector;
    val vector2 = v2.map(x => num.toDouble(x)).toVector; 
    
    math.pow(vector1.zip(vector2).foldLeft(0.0){case(sum,(val1,val2)) => sum + math.pow(val1 - val2, p)}, (1.toDouble/p))
  }
  
  /**
   * Compute cosine distance
   */
  
  /**
   * Find centroid of a Matrix
   */
  def centroid[T](m: Matrix[T])(implicit num: Numeric[T]): Vector[Double] = {
	  m.colMeans; 
  }
  
  /*
   * Find row vector of Matrix that has the smallest distance to the rest of the objects.
   * In other words, find the row vector that is the centroid. 
   */
  def centroidVector[T : Numeric : ClassManifest](m: Matrix[T], method: String = "euclidean"): Vector[T] = {
    // Compute the distances of all the objects and find the object that serves as centroid
    if(m.rows == 1){
      m.takeRow(0);
    }else{
      val distMat = ArrayBuffer.fill(m.rows, m.rows)(0.0);
      for(r <- 0 until m.rows; c <- 0 until m.rows){
        distMat(r)(c) = dist(TtoDouble(m.takeRow(r)), TtoDouble(m.takeRow(c)), method)
      }
      
      val vectorTotalDist = Matrix(distMat.flatten.toArray, m.rows, m.rows).rowSums;

      val vectorMin = vectorTotalDist.min;
      m(vectorTotalDist.indexWhere(x => x == vectorMin))
      
    }
  }
}