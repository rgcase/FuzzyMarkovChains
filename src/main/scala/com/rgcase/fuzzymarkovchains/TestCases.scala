package main.scala.com.rgcase.fuzzymarkovchains

object TestCases {
  
  type Row[T] = Vector[T]
  type Matrix[T] = Vector[Row[T]]
  
  val testVec: Matrix[Double] = Vector(Vector(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1))
  val testVec2: Matrix[Double] = Vector(Vector(1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0))

  val test1: Matrix[Double] = Vector(
      Vector(0.0, 0.0, 0.5, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0), 
      Vector(0.3333333333333333, 0.0, 0.0, 0.3333333333333333, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3333333333333333),
      Vector(0.5, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), 
      Vector(0.0, 0.3333333333333333, 0.0, 0.0, 0.0, 0.3333333333333333, 0.0, 0.3333333333333333, 0.0, 0.0), 
      Vector(0.2, 0.2, 0.0, 0.2, 0.0, 0.0, 0.2, 0.0, 0.2, 0.0), 
      Vector(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), 
      Vector(0.0, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5), 
      Vector(0.3333333333333333,0.0, 0.0, 0.3333333333333333, 0.0, 0.0, 0.3333333333333333, 0.0, 0.0, 0.0), 
      Vector(0.0, 0.2, 0.2, 0.2, 0.0, 0.0, 0.2, 0.0, 0.2, 0.0), 
      Vector(0.0, 0.0, 0.0, 0.5, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0)
      )
}