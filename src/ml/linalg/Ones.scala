package ml.linalg

import scala.collection.mutable.ArrayBuffer
import ml.traits.MatrixT

class Ones[T: Numeric: ClassManifest](r: Int, c: Int) extends MatrixT[T]{
	var rows: Int = r;
	var cols: Int = c; 
	var data = ArrayBuffer.fill(rows, cols)(1.asInstanceOf[T]); 
	
	// define a secondary constructor that takes only one argument
  def this(n: Int) = {
    this(n, n);
  }
}