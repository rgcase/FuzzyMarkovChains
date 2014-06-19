package main.scala.com.rgcase.fuzzymarkovchains

import main.scala.com.rgcase.fuzzymarkovchains.TandSNorms._
import main.scala.com.rgcase.fuzzymarkovchains.TestCases._
import main.scala.com.rgcase.fuzzymarkovchains.Main._


object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(281); 
  println("Welcome to the Scala worksheet")
  
  
  type Norm = (Double, Double) => Double
  type TNorm = Norm
  type SNorm = Norm;$skip(173); 
  
  // Standard Zadeh min/max pair
  def minT: TNorm = (x, y) => if (x < y) x else y;System.out.println("""minT: => (Double, Double) => Double""");$skip(50); 
  def maxS: SNorm = (x, y) => if (x > y) x else y
  
  type Row[T] = Vector[T]
  type Matrix[T] = Vector[Row[T]];System.out.println("""maxS: => (Double, Double) => Double""");$skip(203); 
  
  def transpose(A: Matrix[Double]): Matrix[Double] =
    if (A.head.isEmpty) Vector()
    else A.map(_.head) +: transpose(A.map(_.tail));System.out.println("""transpose: (A: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double])main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double]""");$skip(260); 
  
  def matrixMultWith(A: Matrix[Double], B: Matrix[Double], t: TNorm, s: SNorm): Matrix[Double] = {
    
    for (row <- A) yield
      for (col <- transpose(B)) yield
        (row zip col) map { x: (Double, Double) => t(x._1, x._2) } reduceLeft(s(_,_))
  };System.out.println("""matrixMultWith: (A: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double], B: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double], t: (Double, Double) => Double, s: (Double, Double) => Double)main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double]""");$skip(822); 
  
  val test1: Matrix[Double] = Vector(
      Vector(0.0, 0.0, 0.5, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0),
      Vector(0.3333333333333333, 0.0, 0.0, 0.3333333333333333, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3333333333333333),
      Vector(0.5, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
      Vector(0.0, 0.3333333333333333, 0.0, 0.0, 0.0, 0.3333333333333333, 0.0, 0.3333333333333333, 0.0, 0.0),
      Vector(0.2, 0.2, 0.0, 0.2, 0.0, 0.0, 0.2, 0.0, 0.2, 0.0),
      Vector(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
      Vector(0.0, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2),
      Vector(0.3333333333333333,0.0, 0.0, 0.3333333333333333, 0.0, 0.0, 0.3333333333333333, 0.0, 0.0, 0.0),
      Vector(0.0, 0.2, 0.2, 0.2, 0.0, 0.0, 0.2, 0.0, 0.2, 0.0),
      Vector(0.0, 0.0, 0.0, 0.5, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0)
      );System.out.println("""test1  : main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double] = """ + $show(test1 ));$skip(73); 

  val mat = Vector(
  						Vector(1.0, 2.0),
  						Vector(3.0, 4.0));System.out.println("""mat  : scala.collection.immutable.Vector[scala.collection.immutable.Vector[Double]] = """ + $show(mat ));$skip(73); 
  val testVec2 = Vector(Vector(1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0));System.out.println("""testVec2  : scala.collection.immutable.Vector[scala.collection.immutable.Vector[Double]] = """ + $show(testVec2 ));$skip(90); val res$0 = 
  matrixMultWith(testVec2, test1, (x,y) => if (x<y) x else y, (x,y) => if (x>y) x else y);System.out.println("""res0: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double] = """ + $show(res$0))}
 }
 
 
 