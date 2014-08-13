package ml.cluster

import scala.collection.mutable.ArrayBuffer
import ml.linalg.Matrix
import ml.models.KMeansModel
import ml.stats.statsOps._
import ml.metrics.distance._
import ml.util.TypeConversions._

object Kmeans {
  
  def apply[T: Numeric: ClassManifest] (
		m: Matrix[T],
		k: Int,
		maxIters: Int = 10,
		epsilon: Double = 1e-4,
		distMethod: String = "euclidean",
		p: Int = 2
		): KMeansModel[T] = {
    require(maxIters > 0);
	require(k > 0);
    
	run(m,k,maxIters,epsilon,distMethod,p);
  }
	
	def run[T: Numeric: ClassManifest, num: Numeric](
		m: Matrix[T],
		k: Int,
		maxIters: Int,
		epsilon: Double,
		distMethod: String = "euclidean",
		p: Int = 2
		): KMeansModel[T] = {
	  // generate k random sets of integers to be used as centers
	  var centers = new Array[Array[Double]](k);
	  val allData = m.data.flatten
	  for (kv <- 0 until k){
	    
	    centers(kv) = randomWithin(allData, m.cols).toArray;
	  }
	  
	  // Find the distance between each data point and all the centers
	  var clusters = new Array[Int](m.rows);
	  
	  for(r <- 0 until m.rows){
	      clusters(r) = {
	        var minDist = dist[Double](TtoDouble(m(r)), centers(0), distMethod);
	        var min = 0;
	        for(c <- 1 until k){
	        	var tempNew = dist[Double](TtoDouble(m(r)), centers(c), distMethod);
	        	if(tempNew < minDist){
	        		minDist = tempNew;
	        		min = c;
	        	}
	        	
	        }
	        min;
	    }
	    
	  }
	  
	  val size = clusters.distinct.map(x => (x, clusters.count(_ == x)) ) 
	  new KMeansModel(clusters.toVector, centers, size.toVector, k, maxIters,epsilon, distMethod,p)
	}
}