package ml.tests

import ml.linalg.Matrix
import ml.linalg.Ones
import ml.linalg.Identity
import ml.linalg.Zeros

object test {
	def main(args: Array[String]){
	  
	  //val m1 = new Matrix[Double](4);

	  val m3 = new Matrix[Int](Array(1,2,333,4,5,6,7,9999,9),3,3 );
	  val m4 = new Matrix[Int](Range(1,17).toArray,4,4);
	  val m5 = new Ones[Float](2, 2);
	  val m6 = new Identity[Int](8);
	  val m7 = new Zeros[Int](2,2);
	  
	  println(m4);
	  m4.takeRow(1).foreach(println);
	  println(m3);
	  println(m3((1,1)));
	  m3((1,1)) = 8;
	  m3((2,2)) = 10;
	  println(m3);
	  println(m6);
	  
	}
}