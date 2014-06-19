package main.scala.com.rgcase.fuzzymarkovchains

import main.scala.com.rgcase.fuzzymarkovchains.TandSNorms._
import main.scala.com.rgcase.fuzzymarkovchains.TestCases._
import main.scala.com.rgcase.fuzzymarkovchains.Main._


object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  
  type Norm = (Double, Double) => Double
  type TNorm = Norm
  type SNorm = Norm
  
  // Standard Zadeh min/max pair
  def minT: TNorm = (x, y) => if (x < y) x else y //> minT: => (Double, Double) => Double
  def maxS: SNorm = (x, y) => if (x > y) x else y //> maxS: => (Double, Double) => Double
  
  type Row[T] = Vector[T]
  type Matrix[T] = Vector[Row[T]]
  
  def transpose(A: Matrix[Double]): Matrix[Double] =
    if (A.head.isEmpty) Vector()
    else A.map(_.head) +: transpose(A.map(_.tail))//> transpose: (A: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double])m
                                                  //| ain.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double]
  
  def matrixMultWith(A: Matrix[Double], B: Matrix[Double], t: TNorm, s: SNorm): Matrix[Double] = {
    
    for (row <- A) yield
      for (col <- transpose(B)) yield
        (row zip col) map { x: (Double, Double) => t(x._1, x._2) } reduceLeft(s(_,_))
  }                                               //> matrixMultWith: (A: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Doub
                                                  //| le], B: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double], t: (Dou
                                                  //| ble, Double) => Double, s: (Double, Double) => Double)main.scala.com.rgcase.
                                                  //| fuzzymarkovchains.test.Matrix[Double]
  
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
      )                                           //> test1  : main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double] = Vect
                                                  //| or(Vector(0.0, 0.0, 0.5, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0), Vector(0.33333
                                                  //| 33333333333, 0.0, 0.0, 0.3333333333333333, 0.0, 0.0, 0.0, 0.0, 0.0, 0.33333
                                                  //| 33333333333), Vector(0.5, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), Vec
                                                  //| tor(0.0, 0.3333333333333333, 0.0, 0.0, 0.0, 0.3333333333333333, 0.0, 0.3333
                                                  //| 333333333333, 0.0, 0.0), Vector(0.2, 0.2, 0.0, 0.2, 0.0, 0.0, 0.2, 0.0, 0.2
                                                  //| , 0.0), Vector(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), Vector(0.
                                                  //| 0, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2), Vector(0.3333333333333333,
                                                  //|  0.0, 0.0, 0.3333333333333333, 0.0, 0.0, 0.3333333333333333, 0.0, 0.0, 0.0)
                                                  //| , Vector(0.0, 0.2, 0.2, 0.2, 0.0, 0.0, 0.2, 0.0, 0.2, 0.0), Vector(0.0, 0.0
                                                  //| , 0.0, 0.5, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0))

  val mat = Vector(
  						Vector(1.0, 2.0),
  						Vector(3.0, 4.0))
                                                  //> mat  : scala.collection.immutable.Vector[scala.collection.immutable.Vector[
                                                  //| Double]] = Vector(Vector(1.0, 2.0), Vector(3.0, 4.0))
  val testVec2 = Vector(Vector(1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0))
                                                  //> testVec2  : scala.collection.immutable.Vector[scala.collection.immutable.Ve
                                                  //| ctor[Double]] = Vector(Vector(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
                                                  //| 1.0))
  matrixMultWith(testVec2, test1, (x,y) => if (x<y) x else y, (x,y) => if (x>y) x else y)
                                                  //> res0: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double] = Vector(
                                                  //| Vector(1.0, 0.3333333333333333, 0.5, 0.5, 0.5, 0.3333333333333333, 0.5, 0.3
                                                  //| 333333333333333, 0.2, 0.3333333333333333))
 }
 
 
 