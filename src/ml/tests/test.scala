package ml.tests

import scala.collection.mutable.ArrayBuffer

import ml.linalg.Matrix
import ml.linalg.Ones
import ml.linalg.Identity
import ml.linalg.Zeros
import ml.models.KMeansModel
import ml.metrics.distance._
import ml.cluster.Kmeans
import ml.cluster.KMedoids
import ml.linalg.Table._
import ml.stats.statsOps._
import ml.util.VectorOps._

object test {
	def main(args: Array[String]){

	  val m3 = Matrix(Array(1.0,2,3,5,9,0,1,2,3,5,9,0, 12,23,2, 80, 70, 50),6,3 );
	  val m4 = Matrix(Array(1,2,3,4,4,6,7,8,9,4,5,6), 4,3);
	  println(m3.takeRow(Array(5)));
	  
	  //val kmobj = Kmeans(m3, 2, distMethod="Minkowski", p=2);
	  //println(kmobj);
	  val kmedobj = KMedoids(m3, 2, distMethod="Minkowski", p=2);
	  println(m3);
	  println(kmedobj);
	  
	  println(findIndices(m3(1), 9))
	  
	}
}