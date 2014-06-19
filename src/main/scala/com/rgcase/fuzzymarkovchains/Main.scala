package main.scala.com.rgcase.fuzzymarkovchains

import main.scala.com.rgcase.fuzzymarkovchains.TandSNorms._ 
import main.scala.com.rgcase.fuzzymarkovchains.TestCases._
import main.scala.com.rgcase.fuzzymarkovchains.MatrixOperations._

import java.io.IOException

object Main {

//  val inputInitial = "initialvector.txt"
//  val inputMatrix = "initialmatrix.txt"
//  val sourceInitial = Source.fromFile(inputInitial)
//  val sourceMatrix = Source.fromFile(inputMatrix)
//  
//  val initialVector: Matrix[Double] = transpose(Vector(Vector(1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0)))
//  	Vector(sourceInitial.toString.split(" ").map(_.toDouble).toVector))
//  val initialMatrix: Matrix[Double] = sourceMatrix.getLines.map(
//      _.split(" ").map(_.toDouble).toVector
//    ).toVector
  
  type Row[T] = Vector[T]
  type Matrix[T] = Vector[Row[T]]
  
  def main(args: Array[String]) = {
    
    val M = test1
    val v = testVec
    val vMinMax = Vector(Vector(1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0))
    val size = v(0).length
    

    val correctSize = (M.length == size) && M.map(_.length == size).reduceLeft(_ && _)
    if (!correctSize)
      throw new IOException("The sizes of the matrix and initial vector don't match.")
    
    println("Using initial vector: ")
    println(v)
    println()
    println("Using initial matrix: ")
    M.foreach(println)
    println()
    println("**************************************")
    println()
    
    val timesToApply = 30
    
    println(s"After $timesToApply applications using the usual multiplication and addition.")
    val resultMultAdd = if (timesToApply == 0) M else applyNTimes(M, M, _ * _, _ + _, timesToApply)
    println()
    resultMultAdd.foreach(println)
    println()
    println("Applied to the initial vector:")
    println(matrixMultWith(v, resultMultAdd, _ * _, _ + _)(0))
    println("**************************************")
    println()
    
    println(s"After $timesToApply applications using the min max pair and initial vector: ")
    println(vMinMax)
    val resultMinMax = if (timesToApply == 0) M else applyNTimes(M, M, minT, maxS, timesToApply)
    println()
    resultMinMax.foreach(println)
    println()
    println("Applied to the initial vector:")
    println(matrixMultWith(vMinMax, resultMinMax, minT, maxS))
    println("**************************************")
    println()
    
    println(s"After $timesToApply applications using the product and probabilistic sum pair.")
    val resultProdProb = if (timesToApply == 0) M else applyNTimes(M, M, prodT, probSumS, timesToApply)
    println()
    resultProdProb.foreach(println)
    println()
    println("Applied to the initial vector: ")
    println(matrixMultWith(v, resultProdProb, prodT, probSumS))
    println("**************************************")
    println()
    
    println(s"After $timesToApply applications using the Lukasiewicz and bounded sum pair.")
    val resultLukBnd = if (timesToApply == 0) M else applyNTimes(M, M, lucasT, bndSumS, timesToApply)
    println()
    resultLukBnd.foreach(println)
    println()
    println("Applied to the initial vector: ")
    println(matrixMultWith(v, resultLukBnd, lucasT, bndSumS))
    println("**************************************")
    println()
    
    println(s"After $timesToApply applications using the drastic pair.")
    val resultDrastic = if (timesToApply == 0) M else applyNTimes(M, M, drasT, drasS, timesToApply)
    println()
    resultDrastic.foreach(println)
    println()
    println("Applied to the initial vector: ")
    println(matrixMultWith(v, resultDrastic, drasT, drasS))
    println("**************************************")
    println()
    
    println(s"After $timesToApply applications using the nilpotent pair.")
    val resultNilpotent = if (timesToApply == 0) M else applyNTimes(M, M, nilpotentT, nilpotentS, timesToApply)
    println()
    resultNilpotent.foreach(println)
    println()
    println("Applied to the initial vector: ")
    println(matrixMultWith(v, resultNilpotent, nilpotentT, nilpotentS))
    println("**************************************")
    println()
    
    println(s"After $timesToApply applications using the hamacher and einstein pair.")
    val resultHamEin = if (timesToApply == 0) M else applyNTimes(M, M, hamT, einsteinS, timesToApply)
    println()
    resultHamEin.foreach(println)
    println()
    println("Applied to the initial vector: ")
    println(matrixMultWith(v, resultHamEin, hamT, einsteinS))
    println("**************************************")
  }
  
  
  	
    
}