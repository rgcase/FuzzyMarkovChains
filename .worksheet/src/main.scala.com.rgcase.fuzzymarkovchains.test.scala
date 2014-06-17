package main.scala.com.rgcase.fuzzymarkovchains

import main.scala.com.rgcase.fuzzymarkovchains.TandSNorms._
import main.scala.com.rgcase.fuzzymarkovchains.TestCases._
import main.scala.com.rgcase.fuzzymarkovchains.Main._


object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(281); 
  println("Welcome to the Scala worksheet")
  
  type Norm = (Double, Double) => Double
  type TNorm = Norm
  type SNorm = Norm;$skip(170); 
  
  // Standard Zadeh min/max pair
  def minT: TNorm = (x, y) => if (x < y) x else y;System.out.println("""minT: => (Double, Double) => Double""");$skip(50); 
  def maxS: SNorm = (x, y) => if (x > y) x else y
  
  type Row[T] = List[T]
  type Matrix[T] = List[Row[T]];System.out.println("""maxS: => (Double, Double) => Double""");$skip(592); 
  

  val test1: Matrix[Double] = List(
    List(0, 1.0 / 3, 0.5, 0, 0.2, 1, 0, 1.0 / 3, 0, 0),
    List(0, 0, 0, 1.0 / 3, 0.2, 0, 0, 0, 0.2, 0),
    List(0.5, 0, 0, 0, 0, 0, 0, 0, 0.2, 0),
    List(0, 1.0 / 3, 0.5, 0, 0.2, 0, 0.5, 1.0 / 3, 0.2, 0.5),
    List(0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    List(0, 0, 0, 1.0 / 3, 0, 0, 0, 0, 0, 0),
    List(0, 0, 0, 0, 0.2, 0, 0, 1.0 / 3, 0.2, 0.5),
    List(0, 0, 0, 1.0 / 3, 0, 0, 0, 0, 0, 0),
    List(0, 0, 0, 0, 0.2, 0, 0, 0, 0.2, 0),
    List(0, 1.0 / 3, 0, 0, 0, 0, 0.2, 0, 0, 0)
  );System.out.println("""test1  : main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double] = """ + $show(test1 ));$skip(135); 
  
  def transpose(A: Matrix[Double]): Matrix[Double] =
    if (A.head.isEmpty) Nil
    else A.map(_.head) :: transpose(A.map(_.tail));System.out.println("""transpose: (A: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double])main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double]""");$skip(300); 
  
  def matrixMultWith(A: Matrix[Double], B: Matrix[Double], t: TNorm, s: SNorm): Matrix[Double] = {
    val transB: Matrix[Double] = transpose(B)
    
    for (row <- A) yield
      for (col <- transB) yield
        (row zip col) map { x: (Double, Double) => t(x._1, x._2) } reduceLeft(s(_,_))
  };System.out.println("""matrixMultWith: (A: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double], B: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double], t: (Double, Double) => Double, s: (Double, Double) => Double)main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double]""");$skip(46); val res$0 = 
  
  matrixMultWith(test1, test1, minT, maxS);System.out.println("""res0: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double] = """ + $show(res$0));$skip(228); 
  val start = List(List(1.0),
  								 List(1.0),
  								 List(1.0),
  								 List(1.0),
  								 List(1.0),
  								 List(1.0),
  								 List(1.0),
  								 List(1.0),
  								 List(1.0),
  								 List(1.0));System.out.println("""start  : List[List[Double]] = """ + $show(start ));$skip(19); val res$1 = 
  transpose(start);System.out.println("""res1: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double] = """ + $show(res$1));$skip(46); val res$2 = 
  
  matrixMultWith(test1, start, minT, maxS);System.out.println("""res2: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double] = """ + $show(res$2))}
}
