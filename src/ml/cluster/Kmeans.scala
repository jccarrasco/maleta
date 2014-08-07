package ml.cluster

import scala.collection.mutable.ArrayBuffer

import ml.linalg.Matrix
import ml.models.KMeansModel
import ml.stats.statsOps._
import ml.metrics.distance._

class Kmeans[T: Numeric: ClassManifest] private(
		private var m: Matrix[T],
		private var k: Int,
		private var maxIters: Int,
		private var epsilon: Double
		){
	
	require(maxIters > 0);
	require(k > 0);
	
	def this(data: Matrix[T], k: Int) = {
	  this(data, k, 10, 1e-4);
	}
	
	def run(): KMeansModel[Double] = {
	  // generate k random sets of integers to be used as centers
	  var centers = new Array[Array[Double]](k);
	  for (kval <- 0 until k){
	    centers(k) = randomWithin(m.data.toArray.flatten.asInstanceOf[Vector[Double]], m.cols);
	  }
	  
	  // Find the distance between each data point and all the centers
	  var clusters = new Array[Int](m.rows);
	  
	  for(r <- 0 until m.rows; c <- 1 until k){
	    var maxVal = dist[Double](m.data(r).toArray.asInstanceOf[Array[Double]], centers(0));
	    var max = 0;
	    
	    clusters(r) = {
	      var tempNew = dist[Double](m.data(r).toArray.asInstanceOf[Array[Double]], centers(c));
	      if(tempNew > maxVal){
	        maxVal = tempNew;
	        max = c;
	      }
	      max;
	    }
	  }
	  
	  val size = clusters.distinct.map(x => (x, clusters.count(_ == x)) ) 
	  new KMeansModel(Vector.empty ++ clusters, centers, size)
	}
	
	// Run the algorithm here and return a KMeansModel
	def apply(): KMeansModel[Double] = {
	  run();
	}
}