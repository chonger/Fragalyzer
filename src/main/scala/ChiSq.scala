import multitool._
import scala.collection.mutable._
import java.io._

object L1Analyzer {
  
  def main(args : Array[String]) : Unit = {

    val treeFiles = Array("/home/chonger/data/NLI/icle_u.xml",
                          "/home/chonger/data/NLI/icci_u.xml",
                          "/home/chonger/data/NLI/L8_u.xml",
                          "/home/chonger/data/NLI/fce_u.xml")
    
    val tsgFile = "/home/chonger/data/NLI/naacl_smart.txt"
    val fSave = "/home/chonger/data/NLI/naacl_save.txt"

    val st = new CFGSymbolTable()
    
    val data = treeFiles.map(f => {
      XMLDoc.read(f,st)
    })

    val grammar = PTSG.read(tsgFile,st).rules.toArray.flatMap(_.map(_._1))

/**
    val refRule = st.growTree("(@S (PP <>) (NP <>))").root.rule
    var grammar = getGrammar(Array[String](tsgFile),st,data).filter(x => {
      val rset = new HashSet[TreeRule]() ++ x.nonterminals.map(_.rule).filter(_ != null)
      rset contains refRule
    })

    println("Got " + grammar.length + " rules")
      
    Shuffle(grammar)
    val hm = new HashMap[ParseTree,Double]()
    grammar.foreach(x => hm += x -> 1.0)
    new PTSG(st,hm).write("/home/chonger/data/NLI/naacl_small.txt")
*/

 //   val analyzer = new L1Analyzer(st,data,grammar)
  //  FragInfo.save(fSave,analyzer.fInfo)


    val analyzer = new L1Analyzer(st,data,grammar,fSave)
   
    val useFrags = analyzer.testFilter(15,100,.8,5).map(x => analyzer.frags(x.id)).toArray
    //val useFrags = analyzer.fullFilter(15,100,0.1,1).map(x => analyzer.frags(x.id)).toArray.slice(0,400)
    println(useFrags.length + " left")
    Stats.perFeat(st,data.flatMap(x => x),useFrags.slice(0,400),"/home/chonger/data/NLI/redundancy_testO.txt")

    //best redundancy SU is .5
    //best NPMI is .8

    //Stats.overlap(analyzer,"/home/chonger/data/NLI/NAACLoverlap.txt",400)

  }

  def getGrammar(ptsgs : Array[String], st : CFGSymbolTable, dox : Array[Array[XMLDoc[ParseTree]]]) = {
    val g = new HashSet[ParseTree]()
    ptsgs.foreach(t => {
      g ++= PTSG.read(t,st).rules.toList.flatMap(_.map(_._1))
    })
    val cfgs = TreeTools.cfgSet(dox.flatMap(_.flatMap(_.text).toList).toList).toList
    g ++= cfgs
    println("fragment feature list created : size = " + g.size )
    g.toArray
  }

}

object FragInfo {

  def loadf(s : String,id : Int, a : L1Analyzer) = {
    val p1 = s.trim.split("@")
    val oS = p1(0)
    val aS = p1(1)
    val occ = oS.split(";").map(x => x.split(",").map(_.toInt).toArray).toArray
    val app = new HashSet[Int] ++ aS.split(",").map(_.toInt)
    new FragInfo(id,occ,app,a)
  }

  def load(s : String, a : L1Analyzer) = {
    val r = io.Source.fromFile(s)
    val ret = r.getLines.zipWithIndex.map({
      case (s,i) => {
        loadf(s,i,a)
      }
    }).toArray
    r.close()
    ret
  }

  def save(s : String, infos : Array[FragInfo]) = {
    import java.io._
    val bw = new BufferedWriter(new FileWriter(s))
    infos.foreach(info => {
      bw.write(info.saveString() + "\n")
    })
    bw.close()
  }

}

class FragInfo(val id : Int, val occur : Array[Array[Int]], val appearsIn : HashSet[Int], analyzer : L1Analyzer) {

  def loccur = (Array.tabulate(analyzer.labels.length)(x => 0) /: occur)((a,b) => {
    (a zip b).map(x => x._1 + x._2)
  })


  lazy val su = {
    HELP.su(loccur,analyzer.lTotals)
  }

  lazy val ig = {
    HELP.ig(loccur,analyzer.lTotals)
  }

  lazy val x2 = {
    HELP.chi2(loccur,analyzer.proportionTotals)
  }

  lazy val ax2 = {
    HELP.altchi2(loccur,analyzer.lTotals)
  }

  lazy val ldX2 = {
    HELP.chi2LD(occur)
  }

  lazy val aldX2 = {
    HELP.altchi2LD(occur,analyzer.lcountsA,loccur,analyzer.lTotals)
  }

  def saveString() = {
    occur.map(_.mkString(",")).mkString(";") + "@" + appearsIn.iterator.toArray.mkString(",")
  }

  lazy val loss = {
    0.until(occur.length).map(i => {
      val props = (Array.tabulate(analyzer.labels.length)(x => 0) /: 0.until(occur.length))((a,b) => {
        if(b == i)
          a
        else
          (a zip occur(b)).map(x => x._1 + x._2)
      })
      val propTot = (0.0 /: props)(_ + _)
      val probs = props.map(_/propTot)
      val testTot = (0.0 /: occur(i))(_ + _)
      (0.0 /: (occur(i) zip probs))((a,b) => {
        b match {
          case (count,pr) => {
            a + (1.0 - pr)*(count/testTot)
          }
        }
      })
    }).toArray
  }

  def printMe() = {

    println()

    println(analyzer.frags(id).fString(analyzer.st))

    println()

    println(analyzer.labels.mkString("\t"))
    
    (occur zip analyzer.lcountsA).foreach(x => {
      val y = (x._1 zip x._2).map(z => z._1.toDouble / z._2.toDouble).map(_ * 1000).map("%.1f" format _)
      println(y.mkString("\t"))
    })

    println("---------------------------")
    
    val yy = (loccur zip analyzer.lTotals).map(z => z._1.toDouble / z._2.toDouble).map(_ * 1000).map("%.1f" format _)
    println(yy.mkString("\t"))

    println("AltX2    : " + ax2)
    println("X2       : " + x2)
    println("IG       : " + ig)
    println("SU       : " + su)
    println("ldX2     : " + ldX2) 
    println("AltldX2  : " + aldX2) 

  }

} 

class L1Analyzer(val st : CFGSymbolTable, val dsets :  Array[Array[XMLDoc[ParseTree]]], val frags : Array[ParseTree], saveFile : String) {
    
  def this(st : CFGSymbolTable, dsets :  Array[Array[XMLDoc[ParseTree]]], frags : Array[ParseTree]) {
    this(st,dsets,frags,null)
  }

  val totalDox = (0.0 /: dsets)(_ + _.length)
  val dataProportions = dsets.map(_.length / totalDox).toArray

  println("lengths : " + dsets.map(_.length).toArray.mkString(" "))
  println("dataset proportions : " + dataProportions.mkString(" "))
  
  //get the counts of each L1 in each data set, store in 2D array
  val lcounts : Array[scala.collection.immutable.Map[String,Int]] = dsets.map(dox => {
    dox.groupBy(_.getMeta("goldLabel")).mapValues(_.length)
  }).toArray 

  val labels = (new HashSet[String] ++ lcounts.flatMap(_.keySet)).toArray

  val lcountsA = lcounts.map(x => labels.map(l => x.getOrElse(l,0)).toArray)

  val nLabels = labels.length

  //total number of sentences for each language, summed over all datasets
  val lTotals = (Array.tabulate(labels.length)(x => 0) /: lcounts)((a,b) => {
    val occs = labels.map(l => b.getOrElse(l,0))
    (a zip occs).map(x => x._1 + x._2)
  })
  val proportionTotals = lTotals.map(_ / totalDox)

  val l1Proportions = lcounts.map(lc => {
    val tot = (0.0 /: lc)(_ + _._2)
    labels.map(l => {
      lc.getOrElse(l,0) / tot 
    }).toArray
  })
  println("l1 proportions for each data set: ")
  println(labels.mkString("\t"))
  l1Proportions.foreach(l => {   
    println(l.map("%.2f" format _).mkString("\t"))
  })
  
  val cod = new Compacter(frags.toList)
  val fragInds = new HashMap[ParseTree,Int]()
  val nFrags = frags.length
  0.until(nFrags).foreach(i => {
    fragInds += frags(i) -> i
  })


  val fInfo = if(saveFile == null) {

    println("Finding all overlays in " + totalDox.toInt + " docs")

    //for each sentence, find the rules that appear in it and give it a unique index
    var sInd = 0
    val dox : Array[Array[(XMLDoc[ParseTree],HashSet[Int],Int)]] = dsets.map(_.par.map(d => {
      val tz = new HashSet[ParseTree]()
      val s = d.text(0)
      s.nonterminals.foreach(n => tz ++= cod.findOverlays(n).map(_._1))
      var myInd = -1
      synchronized {
        myInd = sInd
        sInd += 1
        if(sInd % 100 == 0)
          println("Finished " + sInd)
      }
      (d,tz.map(x => fragInds(x)),myInd)
    }).toArray).toArray

    println("Collecting fragment occurence stats")
    var ccc = 0
    0.until(nFrags).map(fI => {
      ccc += 1
      if(ccc % 1000 == 0)
        println("Finished " + ccc)
      val appearsIn = new HashSet[Int]()
      val hasThisFrag = dox.map(_.filter(_._2 contains fI))
      appearsIn ++= hasThisFrag.flatMap(_.map(_._3))
      val grouped = hasThisFrag.map(_.groupBy(_._1.getMeta("goldLabel")))
      val occur : Array[Array[Int]] = grouped.map(ggg => {
        labels.map(x => ggg.getOrElse(x,Array[XMLDoc[ParseTree]]()).length)
      })
      new FragInfo(fI,occur,appearsIn,this)
    }).toArray
  } else {
    println("Loading analysis from " + saveFile)
    FragInfo.load(saveFile,this)
  }    
  println("L1 Analyzer setup complete : " + fInfo.length + " patterns under consideration")

  def redunSU(a : FragInfo, b : FragInfo) = {
    val nA = a.appearsIn.size.toDouble
    val pA = nA/totalDox
    val hA = HELP.ent(nA,totalDox)
    val nB = b.appearsIn.size.toDouble
    val hB = HELP.ent(nB,totalDox)

    val nBandA = (a.appearsIn intersect b.appearsIn).size.toDouble
    val nBnotA = nB - nBandA

    val ig = hB - pA * HELP.ent(nBandA,nA) - (1.0-pA) * HELP.ent(nBnotA,totalDox-nA)

    2*ig/(hA + hB)
  } 

  def redunNPMI(a : FragInfo, b : FragInfo) = {
    val nA = a.appearsIn.size.toDouble
    val nB = b.appearsIn.size
    val pB = nB/totalDox
    val nBandA = (a.appearsIn intersect b.appearsIn).size.toDouble
    val pBgivenA = nBandA / nA         
    
    math.log(pBgivenA/pB) / -math.log(nBandA/totalDox)
  }

  def filterRedundant(fIn : Array[FragInfo], rankF : (FragInfo) => Double, redunF : (FragInfo,FragInfo) => Double, limit : Double) : List[FragInfo] = {

    var sorted = new ArrayBuffer[FragInfo]() ++ fIn.sortWith((a,b) => rankF(a) > rankF(b))

    var good = List[FragInfo]()
    
    while(sorted.length > 0) {
      val top = sorted(0)
      sorted.remove(0)
      val rankV = rankF(top)
      good ::= top
      sorted = sorted.filter(tInfo => {
          redunF(top,tInfo) < limit
      })
    }

    good
  }

  def fullFilter() : List[FragInfo] = {
    fullFilter(15,100,.8,5)
  }

  def fullFilter(minInEachL1 : Double,
                 ldCutoff : Double,
                 redCutoff : Double,
                 relCutoff : Double) : List[FragInfo] = {

    println("filter1")
    
    val firstFilter = fInfo.filter(f => {
      (true /: f.loccur)((a,b) => a && (b > minInEachL1))
    }).filter(_.aldX2 < ldCutoff)

    println("filter2")
    val secondFilter = filterRedundant(firstFilter,_.x2,redunSU,redCutoff)

    println("filter3")
    secondFilter.filter(_.x2 > relCutoff).sortWith(_.x2 > _.x2)

  }

  def redunTEST(a : FragInfo, b : FragInfo) : Double = {

    val npmi = redunSU(a,b)
    
 //   if(npmi > .2)
  //    return 1.0

    if(npmi > -10.0) {

      val sc = SmallestCover(frags(a.id),frags(b.id))     
/**
      if(npmi < .7) {
        a.printMe()
        b.printMe()

        println("SMALLEST COVER")
        if(sc != null) 
          println(sc.pString(st))
        else 
          println("NONE")

        val nA = a.appearsIn.size.toDouble
        val nB = b.appearsIn.size
        val nBandA = (a.appearsIn intersect b.appearsIn).size.toDouble

        val su = redunSU(a,b)

        println("NPMI: " + npmi)
        println("SU  : " + su)

        println("A: " + nA + " B: " + nB + " SAME: " + nBandA)

        readLine()

      }
*/
      if(sc == null) {
        //println("NO OVERLAP")
        0.0
      } else
        1.0
    } else {
      //println("NOT REDUNDANT")
      //println(npmi)
      //a.printMe()
      //b.printMe()
      0.0
    }
  }

  def testFilter(minInEachL1 : Double,
                 ldCutoff : Double,
                 redCutoff : Double,
                 relCutoff : Double) : List[FragInfo] = {

    println("filtering")
    
    val firstFilter = fInfo.filter(f => {
      (true /: f.loccur)((a,b) => a && (b > minInEachL1))
    }).filter(_.aldX2 < ldCutoff)

    println("freq and LD filtered - " + firstFilter.length + " remaining")
    val secondFilter = filterRedundant(firstFilter,_.x2,redunTEST,redCutoff)

    println("redundancy filtered - " + secondFilter.length + " remaining")
    val ret = secondFilter.filter(_.x2 > relCutoff).sortWith(_.x2 > _.x2)

    println("rel filter returning " + ret.length + " frags")
    ret
  }
  

}

