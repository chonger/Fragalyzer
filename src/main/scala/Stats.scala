import multitool._

object Stats {
  import java.io._
  def main(args : Array[String]) = {

  }
  
  //find the overlap of top featuers under different metrics for relevance
  //prints (x,{overlap between the top x features under 2 metrics})
  def overlap(l1Analyzer : L1Analyzer, out : String, nPoints : Int) = {


    import scala.collection.mutable.HashSet
    
    val bw = new BufferedWriter(new FileWriter(out))    

    val chiS = new HashSet[Int]()
    val igS = new HashSet[Int]()
    val suS = new HashSet[Int]()
    
    val xSort = l1Analyzer.fInfo.sortWith(_.x2 > _.x2).toArray.slice(0,nPoints)
    val igSort = l1Analyzer.fInfo.sortWith(_.ig > _.ig).toArray.slice(0,nPoints)
    val suSort = l1Analyzer.fInfo.sortWith(_.su > _.su).toArray.slice(0,nPoints)

    0.until(nPoints).foreach(i => {
      chiS += xSort(i).id
      igS += igSort(i).id
      suS += suSort(i).id
      bw.write(i + " " + (chiS intersect igS).size + " " + (chiS intersect suS).size + " " + (suS intersect igS).size + "\n") 
      bw.flush()
    })

    bw.close()
  }
  
  //per feature expected classification loss using top N under different relevancy metrics
  def perFeat(st : CFGSymbolTable, dox : Array[XMLDoc[ParseTree]], frags : Array[ParseTree], outF : String) = {

    val bw = new BufferedWriter(new FileWriter(outF))

    val nPts = 20
    val step = 20
     
    1.to(nPts).foreach(x => {
      val c = x*step
      if(c <= frags.length) {
        val ff = frags.slice(0,c)
        println("using " + c + " frags")
        val ex = new TSGExtractor(ff.toList,st)
        val d = ex.featsBySent(dox)
        val cl = ex.classifier()
        val loss = cl.lossXval(d,.8,5)
        println(loss)
        bw.write(c + "\t" + loss + "\n")
      }
    })

    bw.close()
  }



/**
  //how does the accuracy (averaged over the test/trains) increase as threshold on LD dep is relaxed?
  def distD() = {
    val ptsgsA = Array("/home/chonger/data/simple3000.txt")
    val bw = new BufferedWriter(new FileWriter("/home/chonger/dump/DIST.txt"))
    
    val chis = 0.until(4).map(i => {
      new ChiSquared(List[Int](i),List[Int](),ptsgsA)  
    })

    val res = 1.to(100).map(x => {
      val cut = 2.5*x
      val axx = (0.0 /: chis)((a,b) => {
        val rules = b.langP.filter(_._4 < cut).map(_._1).toList
        println(cut + " GOT " + rules.length + " Rules")
        val ax = b.acc(rules)
        a + ax / 4.0
      })
      println("\n\n\t\tCUT : " + cut + " ACC : " + axx + "\n\n")

      (cut,axx)
    })

    res.foreach({
      case (cut,axx) => {
        bw.write(cut + " " + axx + "\n")
        bw.flush()
      }
    })

    bw.close()
  }


  //accuracy when limiting by several PMI vs SU redundancy cutoffs
  def red() = {
    val ptsgsA = Array("/home/chonger/data/simple3000.txt")
        
    val chis = 0.until(4).par.map(i => {
      new ChiSquared(List[Int](i),List[Int](),ptsgsA)  
    })

    val mPMI = new HashMap[Double,List[(Double,Int)]]() 
    val mSU = new HashMap[Double,List[(Double,Int)]]()
    
    val cuts = 1.until(20).map(_ * .05)

    chis.foreach(b => {

      cuts.foreach(cut => {
        val pmicut = cut*2 - 1.0
        val nPMI = b.filterRedundant(b.langP.toList.map(x => (x._1,x._2,x._6)),b.totalT,pmicut).toList
        val nSU = b.filterRedundantOld(b.langP.toList.map(x => (x._1,x._2,x._6)),b.totalT,cut).toList

        val aPMI = b.acc(nPMI)
        val aSU = b.acc(nSU)

        synchronized {
          mPMI(cut) = (aPMI,nPMI.length) :: mPMI.getOrElse(cut,List[(Double,Int)]())
          mSU(cut) = (aSU,nSU.length) :: mSU.getOrElse(cut,List[(Double,Int)]())
        }
      })
    })

    var bw = new BufferedWriter(new FileWriter("/home/chonger/dump/RED.txt"))

    cuts.foreach(cut => {
      //val pmiV = (0.0 /: mPMI(cut))(_ +_) / chis.length
      //val suV = (0.0 /: mSU(cut))(_ +_) / chis.length
      (mPMI(cut) zip mSU(cut)).foreach({
        case ((a,b),(c,d)) => bw.write(cut + " " + a + " " + b + " " + c + " " + d + "\n")
      })

    })
    bw.close()
  }

  //gets accuracy with top 300 features under different relevancy metrics
  def getL(tsgF : String, outF : String) = {
    val ptsgsA = Array(tsgF)
    //val ptsgsA = Array("/home/chonger/data/single3-3000.txt")
    val bw = new BufferedWriter(new FileWriter(outF))
    
    val chis = 0.until(4).par.map(i => {
      new ChiSquared(List[Int](i),List[Int](),ptsgsA)  
    })

    val aX = Array.tabulate(2)(x => 0.0)
    val aIG = Array.tabulate(2)(x => 0.0)
    val aSU = Array.tabulate(2)(x => 0.0)
    val norm = 1.0/chis.length.toDouble
    
    chis.foreach(b => {
      val dataF = b.langP
/**
      dataF.toArray.sortWith(_._4 > _._4).slice(0,50).foreach({
        case (r,l,hl,d,hd,ai,ig) => {
          println()
          println(r.fString(b.st))
          println("L: " + hl.toArray.mkString(" "))
          println("D: " + hd.toArray.mkString(" "))
          println("SU: " + ig)
          println("CHI2: " + l)
          println("DATA: " + d)
        }
      })
      throw new Exception()
*/
      val lim = 300
      val xLimit = dataF.sortWith(_._2 > _._2)
      val igLimit = dataF.slice(0,lim).toList
      val suLimit = dataF.sortWith(_._8 > _._8).toList
      val nrX = b.filterRedundant(xLimit.toList.map(x => (x._1,x._2,x._6)),b.totalT).toList.slice(0,lim)
      val nrIG = b.filterRedundant(igLimit.toList.map(x => (x._1,x._7,x._6)),b.totalT).toList.slice(0,lim)
      val nrSU = b.filterRedundant(suLimit.toList.map(x => (x._1,x._8,x._6)),b.totalT).toList.slice(0,lim)

//      val accX = b.accR(nrX)
  //    val accIG = b.accR(nrIG)
    //  val accSU = b.accR(nrSU)      

      synchronized {

        val r1 = b.accR(nrX) 
        val r2 = b.accR(nrIG)
        val r3 = b.accR(nrSU)
        val a1 = b.acc(nrX) 
        val a2 = b.acc(nrIG)
        val a3 = b.acc(nrSU)
        
        bw.write("A " + a1 + " " + a2 + " " + a3 + "\n")
        bw.write("R " + r1 + " " + r2 + " " + r3 + "\n")

        aX(0) += norm*a1
        aX(1) += norm*r1
        aIG(0) += norm*a2
        aIG(1) += norm*r2
        aSU(0) += norm*a3
        aSU(1) += norm*r3

/**
        0.until(21).foreach(i => {
          aX(i) += accX(i) / 4.0
          aIG(i) += accIG(i) / 4.0
          aSU(i) += accSU(i) / 4.0
        })
*/
      }
    })
    bw.write("A " + aX(0) + " " + aIG(0) + " " + aSU(0) + "\n")
    bw.write(" " + aX(1) + " " + aIG(1) + " " + aSU(1) + "\n")

    /**
    0.until(21).foreach(i => {
      val rr = i*.05
      bw.write(rr + " " + aX(i) + " " + aIG(i) + " " + aSU(i) + "\n")
    })
*/
    bw.close()
  }

  def plot() = {
    val ptsgsA = Array("/home/chonger/data/NAACLEVAL/smart1.txt")
    val chi2 = new ChiSquared(List[Int](),List[Int](),ptsgsA)  
    
    import java.io._
/**
    chi2.langP.groupBy(_._2).toArray.sortWith(_._2.length > _._2.length).slice(0,50).foreach(x => {
      println(x._1 + " " + x._2.length)
      x._2.slice(0,10).foreach(y => println(y))
    })
   */
 

    val aIG = (0.0 /: chi2.langP)(_ + _._7) / chi2.langP.length.toDouble
    val aX = (0.0 /: chi2.langP)(_ + _._2) / chi2.langP.length.toDouble
    val aSU = (0.0 /: chi2.langP)(_ + _._8) / chi2.langP.length.toDouble
    val aD = (0.0 /: chi2.langP)(_ + _._4) / chi2.langP.length.toDouble
    val aS = (0.0 /: chi2.langP)(_ + _._6.size) / chi2.langP.length.toDouble
/**
    val bw = new BufferedWriter(new FileWriter("/home/chonger/dump/IGBIAS.txt"))

    chi2.langP.foreach(x => {
      bw.write(x._7 + " " + x._2 + " " + x._6.size + "\n")
    })

    bw.close()
*/
    
    val dataF = chi2.langP

    

    val xLimit = dataF.sortWith(_._8 > _._8)
//    val nrX = chi2.filterRedundantOld(xLimit.toList.map(x => (x._1,x._8,x._6)),chi2.totalT).toLis
    val nrX = chi2.filterRedundant(xLimit.toList.map(x => (x._1,x._2,x._6)),chi2.totalT,.8).toList

    println("NRX : " + nrX.size)

    val mmm = new HashMap[ParseTree,(ParseTree,Double,HashMap[String,Int],Double,HashMap[(String,Int),Int],HashSet[Int],Double,Double)]()
    xLimit.foreach(x => {
      mmm += x._1 -> x
    })

    var bw = new BufferedWriter(new FileWriter("/home/chonger/dump/X.txt"))

    nrX.map(x => mmm(x)).sortWith(_._2 > _._2).foreach({
      case (r,l,hl,d,hd,ai,ig,suu) => {
        bw.write("\n")
        bw.write(r.fString(chi2.st) + "\n")
          bw.write("L: " + hl.toArray.mkString(" ") + "\n")
          bw.write("D: " + hd.toArray.map({
            case ((a,b),c) => a + "-" + b + "-" + c
          }).mkString(" ") + "\n")
          bw.write("SU: " + suu + "\n")
          bw.write("IG: " + ig + "\n")
          bw.write("CHI2: " + l + "\n")
          bw.write("DATACHI2: " + d + "\n")
        }
    })

    bw.close()


    def corr(xs : Array[Double], ys : Array[Double], xb : Double, yb : Double) = {
      val aa = (0.0 /: (xs zip ys))((a,b) => a + (b._1 - xb) * (b._2 - yb))
      val bb = math.sqrt((0.0 /: xs)((a,b) => a + math.pow((b - xb),2)))
      val cc = math.sqrt((0.0 /: ys)((a,b) => a + math.pow((b - yb),2)))
      aa/(bb*cc)
    }

    val igs = chi2.langP.map(_._7).toArray
    val chis = chi2.langP.map(_._2).toArray
    val sus = chi2.langP.map(_._8).toArray
    val ds = chi2.langP.map(_._4).toArray
    val sizes = chi2.langP.map(_._6.size.toDouble).toArray

    println("D CORR: " + corr(ds,sizes,aD,aS))
    println("SU CORR: " + corr(sus,sizes,aSU,aS))
    println("IG CORR: " + corr(igs,sizes,aIG,aS))
    println("X CORR: " + corr(chis,sizes,aX,aS))

  }
  

  def classify() = {

    cc.mallet.util.MalletProgressMessageLogger.getLogger("MalletProgressMessageLogger").setLevel(java.util.logging.Level.OFF)
    cc.mallet.util.MalletProgressMessageLogger.getLogger("LimitedMemoryBFGS").setLevel(java.util.logging.Level.OFF)

    val ptsgsA = Array("/home/chonger/data/simple3000.txt")
    val ret = 0.until(4).map(ind => {
      val testI = List[Int](ind)
      val chi2 = new ChiSquared(testI,List[Int](),ptsgsA)  
      chi2.investigateOld(6.3,50,20)
    })

    ret.foreach(x => println(x))

  }

  def compare() = {

    val bw = new BufferedWriter(new FileWriter("/home/chonger/dump/COMP.txt"))

    val fbase = "/home/chonger/data/NAACLEVAL/"

    val types = Array("single","simple","smart","lda")

    val cuts = Array(6.25,7.82,11.34,16.27)

    val outs = types.map(t => {
      val res = 1.to(5).flatMap(ind => {
        try {
          List(compareD(fbase+t+ind+".txt",cuts))
        } catch {
          case _ => List[Array[Double]]()
        }
      })
      
      val norm = 1.0 / res.length.toDouble
      val avg = (res(0).map(x => 0.0) /: res)((a,b) => {
        (a zip b).map(x => x._1 + x._2 * norm)
      })
      bw.write(t + " " + avg.mkString(" ") + "\n")
    })
    bw.close()
  }

  def compareD(inF : String, cuts : Array[Double]) : Array[Double]= {
    val ptsgsA = Array(inF)

    val testI = List[Int]()
    val devI = List[Int]()

    val chi2 = new ChiSquared(testI,devI,ptsgsA)


    val mmmA = new HashMap[ParseTree,(ParseTree,Double,HashMap[String,Int],Double,HashMap[(String,Int),Int],HashSet[Int],Double,Double)]()
    chi2.langP.foreach(x => {
      mmmA += x._1 -> x
    })

    val fs = chi2.filterRedundant(chi2.langP.toList.map(x => (x._1,x._2,x._6)),chi2.totalT)

    val chisA = fs.map(x => {
      mmmA(x)._2
    })

    
    (cuts.map(s => { 
      chisA.filter(_ >= s).length.toDouble / chi2.langP.length.toDouble
    }).toList  ::: cuts.map(s => { 
      chisA.filter(_ >= s).length.toDouble / chi2.grammar.length.toDouble
    }).toList).toArray
  }

  def acc(grammar : List[ParseTree]) = {
    if(testDox.length > 0) {
      val pcfgex = new TSGExtractor(grammar,st)
      val pcfgI1 = pcfgex.featsBySent(trainDox.flatMap(x=>x))
      val pcfgI2 = pcfgex.featsBySent(testDox.flatMap(x=>x))
      val pcfgC = pcfgex.classifier(.1)
      
      val insts1 = pcfgC.makeInsts(pcfgI1)
      val insts2 = pcfgC.makeInsts(pcfgI2)
      
      pcfgC.eval2(insts1,insts2)
    } else 
      0.0
  }

  def accG(grammar : List[ParseTree]) = {
    if(testDox.length > 0) {
      val pcfgex = new TSGExtractor(grammar,st)
      val pcfgI1 = pcfgex.featsBySent(trainDox.flatMap(x=>x))
      val pcfgI2 = pcfgex.featsBySent(testDox.flatMap(x=>x))
      val pcfgC = pcfgex.classifier(.1)
      
      val insts1 = pcfgC.makeInsts(pcfgI1)
      val insts2 = pcfgC.makeInsts(pcfgI2)
      
      pcfgC.eval2(insts1,insts2)
    } else 
      0.0
  }

  def accR(grammar : List[ParseTree]) = {
    if(testDox.length > 0) {
      val pcfgex = new TSGExtractor(grammar,st)
      val pcfgI1 = pcfgex.featsBySent(trainDox.flatMap(x=>x))
      val pcfgI2 = pcfgex.featsBySent(testDox.flatMap(x=>x))
      val pcfgC = pcfgex.classifier(.1)
      
      val insts1 = pcfgC.makeInsts(pcfgI1)
      val insts2 = pcfgC.makeInsts(pcfgI2)
      
      pcfgC.evalR(insts1,insts2,1)
    } else 
      0.0
  }
*/

}
