import multitool._
import enbuske._

object SemiSampler {

  val fBase = "/home/chonger/data/ICLE/"
  val f1 = "/home/chonger/data/ICLE/icle_u.xml"
  val f2 = "/home/chonger/data/ICC/xml/icci_u.xml"
  val nIter = 100
  val pp = "semi-pp.txt"
  val smp = "semi-sampled.txt"

  def main(args : Array[String]) = {

    new SampleDiagonal(1000)(fBase,f1,f2,nIter,pp,smp)

  }

}
