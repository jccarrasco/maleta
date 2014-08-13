package ml.models

import ml.traits.ModelT
import ml.linalg.Table._

class KMedoidsModel[T: Numeric: ClassManifest](
    val clusters: Vector[Int], 
    val centers: Array[Array[T]], 
    val size: Vector[(Int,Int)],
    val k: Int,
    val maxIters: Int,
    val epsilon: Double,
    val distMethod: String,
    val p: Int
		) extends ModelT{
  
  override def toString = {
    val rv = new scala.StringBuilder;
    
    rv.append("\n KMedoids---\n");
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