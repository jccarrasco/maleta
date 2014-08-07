package ml.linalg
import ml.traits.TableT

object Table {
  /**
   * Create a cross tabulation
   */	
  def table[T](vector: Vector[T])(implicit m: ClassManifest[T]): Table[T] = {
    new Table(vector.distinct.map(x => (x, vector.count(_ == x)) )) 
  }
}

class Table[T: ClassManifest](t: Vector[(T, Int)]) extends TableT[T]{
  var data: Vector[(T, Int)] = t;
}