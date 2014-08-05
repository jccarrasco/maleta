package ml.linalg

import scala.collection.mutable.ArrayBuffer

class Zeros[T: Numeric: ClassManifest](r: Int, c: Int) extends MatrixT [T]{
	var cols: Int = r;
	var rows: Int = c;
	var data = ArrayBuffer.fill(rows, cols)(0.asInstanceOf[T]);
	
	// define a secondary constructor that takes only one argument
  	def this(n: Int) = {
  		this(n, n);
  	}
}