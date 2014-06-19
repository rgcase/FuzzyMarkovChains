package main.scala.com.rgcase.fuzzymarkovchains

object TandSNorms {

  type Norm = (Double, Double) => Double
  type TNorm = Norm
  type SNorm = Norm
  
  // Standard Zadeh min/max pair
 def minT: TNorm = (x, y) => if (x < y) x else y
 def maxS: SNorm = (x, y) => if (x > y) x else y
  
  // Product and Probabilistic sum pair
  def prodT: TNorm = (x, y) => x * y
  def probSumS: SNorm = (x, y) => ((x + y) - (x * y))
  
  // Lukasiewicz and Bounded Sum pair
  def lucasT: TNorm = (x, y) => maxS(0, x + y - 1)
  def bndSumS: SNorm = (x, y) => minT(x + y, 1)
  
  // Drastic T and S pair
  def drasT: TNorm = (x, y) => if (x == 1) y else if (y == 1) x else 0
  def drasS: SNorm = (x, y) => if (x == 0) y else if (y == 0) x else 1
  
  // Nilpotent 
  def nilpotentT: TNorm = (x, y) => if (x + y > 1) minT(x, y) else 0
  def nilpotentS: SNorm = (x, y) => if (x + y < 1) maxS(x,y) else 1
  
  // Hamacher
  def hamT: TNorm = (x, y) => if (x == 0 && y == 0) 0 else (x + y) / probSumS(x,y)
  def einsteinS: SNorm = (x, y) => ((x + y) / (1 + prodT(x,y)))
  
//  def dual(t: Norm): Norm = t match {
//    case minT => `maxS`
//    case maxS => `minT`
//    case prodT => `probSumS`
//    case probSumS => `prodT`
//    case lucasT => `bndSumS`
//    case bndSumS => `lucasT`
//    case drasT => `drasS`
//    case drasS => `drasT`
//    case nilpotentT => `nilpotentS`
//    case nilpotentS => `nilpotentT`
//    case hamT => `einsteinS`
//    case einsteinS => `hamT`
//    case _ => throw new NoSuchElementException("No dual function")
//  }
  
  
  
}