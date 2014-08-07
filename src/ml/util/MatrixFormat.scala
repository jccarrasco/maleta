package ml.util

import scala.collection.mutable.ArrayBuffer

object MatrixFormat {
  
  // get largest number of digits 
  def maxWidth[@specialized T](d: ArrayBuffer[T]): Int = {
    val newd = d map(_.toString);
    
    val widths = newd map(_.length());
    val maxw = widths.max;
    maxw;
  }
  
  // get largest number of digits 
  def maxWidth[@specialized T](d: List[T]): Int = {
    val newd = d map(_.toString);
    
    val widths = newd map(_.length());
    val maxw = widths.max;
    maxw;
  }

}