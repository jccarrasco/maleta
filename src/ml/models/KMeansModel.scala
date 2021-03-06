package ml.models

import ml.linalg.Table._
import ml.traits.ModelT


/**
 * A Kmeans Model Object that contains information about a particular model
 * 
 * @constructor create a new model with clusters, centers, withinss, betweeness, and size
 * @param clusters vector containing the cluster to which each element belongs
 * @param centers vector containing the center of each cluster found
 * @param size vector of size of each cluster
 */
class KMeansModel[T: Numeric: ClassManifest](
    val clusters: Vector[Int], 
    val centers: Array[Array[Double]], 
    val size: Vector[(Int,Int)],
    val k: Int,
    val maxIters: Int,
    val epsilon: Double,
    val distMethod: String,
    val p: Int
		) extends ModelT{
  
  override def toString = {
    val rv = new scala.StringBuilder;
    
    rv.append("\n KMeans---\n");
    rv.append("K: "+k);
    rv.append("\nmaxIters: "+ maxIters);
    rv.append("\nepsilon: " +epsilon);
    rv.append("\ndistMethod: "+distMethod);
    if(distMethod.toUpperCase == "MINKOWSKI")rv.append("\np: "+p);
    rv.append("\n\nClusters: \n" + clusters + "\n");
    rv.append("\nCenters:\n");
    
    for(r <- 0 until centers.size){
      rv.append(centers(r).toVector+"\n")
    }
    
    rv.append("\n\nSize: \n");
    rv.append(tableR(size))
    
    rv.toString;
  }
	
}