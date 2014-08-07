package ml.tests

import ml.linalg.Matrix
import ml.linalg.Ones
import ml.linalg.Identity
import ml.linalg.Zeros
import ml.models.KMeansModel
import ml.metrics.distance._
import ml.cluster.Kmeans
import ml.linalg.Table._

object test {
	def main(args: Array[String]){

	  val m3 = new Matrix[Int](Array(1,2,3,1,2,3,5,9,0),3,3 );
	  
	  val kmobj = new Kmeans(m3, 2);
	  //println(kmobj.run.clusters);
	  println(table(Vector("a",3,4,5022,3,3,4,"a",1,6,428,9)))
	}
}