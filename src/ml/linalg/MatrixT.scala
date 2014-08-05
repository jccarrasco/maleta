package ml.linalg

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import ml.util.MatrixFormat

trait MatrixT[@specialized T]{
  var rows: Int;
  var cols: Int;
  var data: ArrayBuffer[ArrayBuffer[T]];
  
  /*
   * Return dimensions of Matrix
   */
  def dim: (Int, Int) = (rows, cols);
    
  // rbind
  
  // cbind
  
   // Override toString method
  override def toString = {
    // Obtain max width to be considered
    val mtoA = data.flatten;
    val max =  MatrixFormat.maxWidth(mtoA);
    
    val rv = new scala.StringBuilder;
    for(r <- 0 until rows; c <- 0 until cols){
      rv.append(" "*(max-(data(r)(c).toString.length) + 1) + data(r)(c));
      
      if(c == cols-1){
        rv.append("\n");
      }
    }
    rv.toString;
  }

}