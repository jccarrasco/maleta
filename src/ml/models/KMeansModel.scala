package ml.models


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
    val centers: Array[Array[T]], 
    val size: Array[(Int,Int)]) {
	
}