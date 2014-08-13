package ml.cluster

import scala.collection.mutable.ArrayBuffer
import ml.linalg.Matrix
import ml.models.KMedoidsModel
import ml.stats.statsOps._
import ml.metrics.distance._
import ml.util.TypeConversions._
import ml.util.VectorOps._

object KMedoids {

	def apply[T: Numeric: ClassManifest] (
		m: Matrix[T],
		k: Int,
		maxIters: Int = 10,
		epsilon: Double = 1e-4,
		distMethod: String = "euclidean",
		p: Int = 2
		): KMedoidsModel[T] = {
    require(maxIters > 0);
	require(k > 0);
	
	run(m, k, maxIters, epsilon, distMethod, p);
	
	}
	
	def run[T: Numeric: ClassManifest, num: Numeric](
		m: Matrix[T],
		k: Int,
		maxIters: Int,
		epsilon: Double,
		distMethod: String = "euclidean",
		p: Int = 2
		): KMedoidsModel[T] = {
	  
	  var iteration = 0;
	  var distSim = Double.MaxValue;	// distance similarity
	  var centers = new Array[Array[T]](k);
	  var clusters = new Array[Int](m.rows);
	  var distances = new Array[Double](m.rows);
	  
	  var bestCenters = centers;
	  var bestClusters = clusters;
	  var bestDistances = distances;
	  
	  
	  while(iteration < maxIters){ //&& distSim <= epsilon){
		  
	    if(iteration < 1){
	      // chosose k randomly selected row vectors from matrix m and use as initial centers
		  val randVals = randomInts(0, m.rows , k);
		  var iter = 0;
	  
		  for(kv <- 0 until k){
			  centers(kv) = m.takeRow(randVals(iter)).toArray;
			  iter = iter + 1;
		  }
	    }
	    else{
	      
	      for(kv <- clusters.distinct){
	        // Find indices that match the given cluster
	        val indices = findIndices(clusters, kv);
	        // Take rows from matrix, apply centroidVector and store in centers again
	        centers(kv) = centroidVector(m.takeRow(indices), distMethod).toArray;
	      }
	        
	    }
		  // Find the distance between each data point and all the centers
		  for(r <- 0 until m.rows){
			 val clust = {
				  var minDist = dist[Double](TtoDouble(m(r)), TtoDouble(centers(0)), distMethod);
				  var min = 0;
				  for(c <- 1 until k){
					  var tempNew = dist[Double](TtoDouble(m(r)), TtoDouble(centers(c)), distMethod);
					  if(tempNew < minDist){
						  minDist = tempNew;
						  min = c;
					  }
	        	
				  }
				  (min, minDist);
			  }
			  
			  clusters(r) = clust._1;
			  distances(r) = clust._2;
		  }
		  
		  if(distances.sum < distSim){
		    bestCenters = centers;
		    bestClusters = clusters;
		    bestDistances = distances;
		    distSim = bestDistances.sum;
		    
		    //println("Best distance: "+bestDistances.sum);
		    //println("Current distance: " + distances.sum)
		  }
		  //println("Best in Iter: "+iteration)
		  iteration += 1;
	  }
	  
	  val size = bestClusters.distinct.map(x => (x, bestClusters.count(_ == x)) )
	  new KMedoidsModel(bestClusters.toVector, bestCenters, size.toVector,k, maxIters,epsilon, distMethod,p);
	}
	
}