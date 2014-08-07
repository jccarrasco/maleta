package ml.linalg

import ml.traits.MatrixT

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.collection.immutable.Vector
import scala.collection.immutable.VectorBuilder

import ml.stats.statsOps._

object Matrix{
  def apply[T: Numeric](r: Int, c: Int)(implicit m: ClassManifest[T]): Matrix[T] = Matrix(r, c);
  def apply[T: Numeric](r: Int, c: Int, rand: Random)(implicit m: ClassManifest[T]): Matrix[Int] =
    new Matrix(Array.fill[Int](r * c)(rand.nextInt()), r, c);
  
  def apply[T](value: T) = value;
}

class Matrix[T: Numeric: ClassManifest](d: Array[T], r: Int, c: Int) extends MatrixT[T]{

  var rows: Int = r;
  var cols: Int = c;
  var data = {
    
      if(d.length != rows * cols){
    	  throw new IllegalArgumentException
      }
      
      // Create an array of arrays
      var m = ArrayBuffer.fill(rows, cols)(0.asInstanceOf[T]);
      var init = 0;
      
      for( r <- 0 until rows; c <- 0 until cols){
        m(r)(c) = d(init);
        init += 1;
      }
      m;
    
  }; 
  
  // define a secondary constructor that takes only one argument
  def this(n: Int) = {
    this(Array.fill[T](n * n)(0.asInstanceOf[T]), n, n);
  }
  
  // define a third constructor that takes number of rows and columns
  def this(r: Int, c: Int){
    this(Array.fill[T](r * c)(0.asInstanceOf[T]), r, c);
  }
  
  // define a fourth constructor that takes  Iterable and diensions.
  def this(v: Iterable[T], r: Int, c: Int){
    this(v.toArray, r, c);
  }
  
  // Assign new value in the given row and column
  // Use apply and update for a more powerful for of update in a matrix
  def update(rc: (Int, Int), value: T): Unit = {
    
    if(rc._1 < rows && rc._1 >= 0 && rc._2 < cols && rc._2 >= 0){
      data(rc._1)(rc._2) = value;
    }
    else{
      throw new IndexOutOfBoundsException("Index out of bounds.");
    }
    
  }
  
  // bind a new vector row-wise
  def rbind(vector: Vector[T]) = {
    if(vector.length != cols){
      throw new IndexOutOfBoundsException("Dimensions do not match.")
    }else{
      rows += 1;
            
      var newdata = data.toArray.flatten ++ vector;
      
      // Create an array of arrays
      var m = ArrayBuffer.fill[T](rows, cols)(0.asInstanceOf[T]);
      var init = 0;
      
      for( r <- 0 until rows; c <- 0 until cols){
        m(r)(c) = newdata(init);
        init += 1;
      }
      this.data = m;
    }
    this;
  }
  
  // bind a new vector row-wise
  def cbind(vector: Vector[T]) = {
    if(vector.length != rows){
      throw new IndexOutOfBoundsException("Dimensions do not match.")
    }else{
      cols += 1;
            
      var newdata = data.toArray.flatten;
      
      // Create an array of arrays
      var m = ArrayBuffer.fill[T](rows, cols)(0.asInstanceOf[T]);
      var m_i = 0;	// keep track of existing elements of matrix
      var v_i = 0;	// keep track of new elements to be added
      
      for( r <- 0 until rows; c <- 0 until cols){
        
        if(c == cols-1){
          m(r)(c) = vector(v_i);
          v_i += 1;
        }else{
          m(r)(c) = newdata(m_i);
          m_i += 1;
        }
      }
      this.data = m;
    }
    this;
  }
  
  // Take row
  def takeRow(r: Int): Vector[T] = {
    data(r).toVector;
  }
  
  // Take rows
  /*def takeRow(list: Iterable[Int]): Matrix[T] = {
    var vector =ArrayBuffer[ArrayBuffer[T]]();
    for(r <- list){
      vector = vector :+ data(r);
    }
    Matrix[T](list.size, cols);
  } */
  
  // Take column
  def takeCol(c: Int): Vector[T] = {
    var vector = Vector[T]();
    for(r <- 0 until rows){
      vector = vector :+ data(r)(c);
    }
    vector;
  }
  
  // Row means
  def rowMeans(): Vector[Double] = {
    var mv = Vector[Double]();
    for(r <- 0 until rows){
      mv = mv :+ mean[T](takeRow(r));
    }
    mv;
  }
  
  // Col means
  def colMeans(): Vector[Double] = {
    var mv = Vector[Double]();
    for(c <- 0 until cols){
      mv = mv :+ mean[T](takeCol(c));
    }
    mv;
  }
  
  
  // Row sum
  def rowSums(): Vector[Double] = {
    var mv = Vector[Double]();
    for(r <- 0 until rows){
      mv = mv :+ sum[T](takeRow(r));
    }
    mv;
  }
  
  // Col sum
  def colSums(): Vector[Double] = {
    var mv = Vector[Double]();
    for(c <- 0 until cols){
      mv = mv :+ sum[T](takeCol(c));
    }
    mv;
  }
  
  //Create a Matrix (list of lists)
  def apply(rc: (Int, Int)): T = {
    data(rc._1)(rc._2);
  }
  
}