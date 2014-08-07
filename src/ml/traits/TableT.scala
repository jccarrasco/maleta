package ml.traits

import ml.util.MatrixFormat._

trait TableT[@specialized T] {
	var data: Vector[(T, Int)];
	
	override def toString = {
	  // Obtain max width to be considered
	  val mtoA = Array.empty ++ data.map(_._1);
	  val mtoA2 = Array.empty ++ data.map(_._2);
	  val max1 =  maxWidth(mtoA.toList);
	  val max2 = maxWidth(mtoA2.toList);
	  val max = math.max(max1, max2);
	  
	  val rows = 2;
	  val cols = data.size;
	  
	  val rv = new scala.StringBuilder;
	  for( c <- 0 until cols){
	    rv.append(" "*(max-(data(c)._1.toString.length) + 1) + data(c)._1);
      }
	  rv.append("\n");
	  for( c <- 0 until cols){
	    rv.append("-"*(max-(data(c)._2.toString.length) + 1) + "-");
      }
	  rv.append("\n");
	  for( c <- 0 until cols){
	    rv.append(" "*(max-(data(c)._2.toString.length) + 1) + data(c)._2);
      }
      
	  rv.toString;
	}
}