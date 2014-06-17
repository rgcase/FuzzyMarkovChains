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
  
  type Row[T] = List[T]
  type Matrix[T] = List[Row[T]]
  

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
  )                                               //> test1  : main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double] = List
                                                  //| (List(0.0, 0.3333333333333333, 0.5, 0.0, 0.2, 1.0, 0.0, 0.3333333333333333,
                                                  //|  0.0, 0.0), List(0.0, 0.0, 0.0, 0.3333333333333333, 0.2, 0.0, 0.0, 0.0, 0.2
                                                  //| , 0.0), List(0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.0), List(0.0, 0
                                                  //| .3333333333333333, 0.5, 0.0, 0.2, 0.0, 0.5, 0.3333333333333333, 0.2, 0.5), 
                                                  //| List(0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), List(0.0, 0.0, 0.0,
                                                  //|  0.3333333333333333, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), List(0.0, 0.0, 0.0, 0.0
                                                  //| , 0.2, 0.0, 0.0, 0.3333333333333333, 0.2, 0.5), List(0.0, 0.0, 0.0, 0.33333
                                                  //| 33333333333, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), List(0.0, 0.0, 0.0, 0.0, 0.2, 0
                                                  //| .0, 0.0, 0.0, 0.2, 0.0), List(0.0, 0.3333333333333333, 0.0, 0.0, 0.0, 0.0, 
                                                  //| 0.2, 0.0, 0.0, 0.0))
  
  def transpose(A: Matrix[Double]): Matrix[Double] =
    if (A.head.isEmpty) Nil
    else A.map(_.head) :: transpose(A.map(_.tail))//> transpose: (A: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double])
                                                  //| main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double]
  
  def matrixMultWith(A: Matrix[Double], B: Matrix[Double], t: TNorm, s: SNorm): Matrix[Double] = {
    val transB: Matrix[Double] = transpose(B)
    
    for (row <- A) yield
      for (col <- transB) yield
        (row zip col) map { x: (Double, Double) => t(x._1, x._2) } reduceLeft(s(_,_))
  }                                               //> matrixMultWith: (A: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Dou
                                                  //| ble], B: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double], t: (D
                                                  //| ouble, Double) => Double, s: (Double, Double) => Double)main.scala.com.rgca
                                                  //| se.fuzzymarkovchains.test.Matrix[Double]
  
  matrixMultWith(test1, test1, minT, maxS)        //> res0: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double] = List(Li
                                                  //| st(0.5, 0.0, 0.0, 0.3333333333333333, 0.2, 0.0, 0.0, 0.0, 0.2, 0.0), List(0
                                                  //| .2, 0.3333333333333333, 0.3333333333333333, 0.0, 0.2, 0.0, 0.33333333333333
                                                  //| 33, 0.3333333333333333, 0.2, 0.3333333333333333), List(0.0, 0.3333333333333
                                                  //| 333, 0.5, 0.0, 0.2, 0.5, 0.0, 0.3333333333333333, 0.2, 0.0), List(0.5, 0.33
                                                  //| 33333333333333, 0.0, 0.3333333333333333, 0.2, 0.0, 0.2, 0.3333333333333333,
                                                  //|  0.2, 0.5), List(0.0, 0.3333333333333333, 0.5, 0.0, 0.2, 0.5, 0.0, 0.333333
                                                  //| 3333333333, 0.0, 0.0), List(0.0, 0.3333333333333333, 0.3333333333333333, 0.
                                                  //| 0, 0.2, 0.0, 0.3333333333333333, 0.3333333333333333, 0.2, 0.333333333333333
                                                  //| 3), List(0.2, 0.3333333333333333, 0.0, 0.3333333333333333, 0.2, 0.0, 0.2, 0
                                                  //| .0, 0.2, 0.0), List(0.0, 0.3333333333333333, 0.3333333333333333, 0.0, 0.2, 
                                                  //| 0.0, 0.3333333333333333, 0.3333333333333333, 0.2, 0.3333333333333333), List
                                                  //| (0.2, 0.0, 0.0, 0.0, 0.
                                                  //| Output exceeds cutoff limit.
  val start = List(List(1.0),
  								 List(1.0),
  								 List(1.0),
  								 List(1.0),
  								 List(1.0),
  								 List(1.0),
  								 List(1.0),
  								 List(1.0),
  								 List(1.0),
  								 List(1.0))
                                                  //> start  : List[List[Double]] = List(List(1.0), List(1.0), List(1.0), List(1.
                                                  //| 0), List(1.0), List(1.0), List(1.0), List(1.0), List(1.0), List(1.0))
  transpose(start)                                //> res1: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double] = List(Li
                                                  //| st(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0))
  
  matrixMultWith(test1, start, minT, maxS)        //> res2: main.scala.com.rgcase.fuzzymarkovchains.test.Matrix[Double] = List(Li
                                                  //| st(1.0), List(0.3333333333333333), List(0.5), List(0.5), List(0.5), List(0.
                                                  //| 3333333333333333), List(0.5), List(0.3333333333333333), List(0.2), List(0.3
                                                  //| 333333333333333))
}