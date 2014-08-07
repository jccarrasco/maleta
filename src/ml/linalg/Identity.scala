package ml.linalg

import scala.collection.mutable.ArrayBuffer
import ml.traits.MatrixT

class Identity[T: Numeric: ClassManifest](n: Int) extends MatrixT[T]{
	var rows: Int = n;
	var cols: Int = n;
	var data = {
	  var m = ArrayBuffer.fill(rows, cols)(0.asInstanceOf[T]);
	
	  for(r <- 0 until rows; c <- 0 until cols){
		  if(r == c){
			  m(r)(c) = 1.asInstanceOf[T];
		  }
	  }
	  m;
	}
}