package main.scala.com.rgcase.fuzzymarkovchains

import java.io.IOException

object MatrixOperations {

  type Row[T] = Vector[T]
  type Matrix[T] = Vector[Row[T]]
  
  type Norm = (Double, Double) => Double
  type TNorm = Norm
  type SNorm = Norm
  
  def transpose(A: Matrix[Double]): Matrix[Double] = 
    if (A.head.isEmpty) Vector()
    else A.map(_.head) +: transpose(A.map(_.tail))
  
  def matrixMultWith(A: Matrix[Double], B: Matrix[Double], t: TNorm, s: SNorm): Matrix[Double] = {
    
    for (row <- A) yield
      for (col <- transpose(B)) yield
        (row zip col) map { x: (Double, Double) => t(x._1, x._2) } reduceLeft(s(_,_))
  }
  
  def applyNTimes(A: Matrix[Double], B: Matrix[Double], t: TNorm, s: SNorm, times: Int): Matrix[Double] = 
    times match {
      case _ if times < 1 => throw new IOException("applyNTimes called with times < 1")
      case 1 => matrixMultWith(A, B, t, s)
      case n => applyNTimes(normalize(matrixMultWith(A, B, t, s)), B, t, s, n-1)
    } 
  
  def normalize(A: Matrix[Double]): Matrix[Double] = {
    
    if (A.length > 1) throw new IllegalArgumentException("normalize called with non-row vector")
    
    val row = A(0)
    val (max, min): (Double, Double) = row.foldLeft((1.0,0.0)) {
      (pair, x: Double) => (if (x > pair._1) x else pair._1, if (x > pair._2) pair._2 else x)
    }
    
    Vector(row.map { x => if (max - min == 0.0) 0.0 else (x - min)/(max - min)})
  }
  
}