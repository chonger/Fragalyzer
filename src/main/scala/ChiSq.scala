import multitool._
import scala.collection.mutable._


object ChiSquared {

  def pUniform(m : Array[Int]) = {
    val alpha = 1.0
    
    import cc.mallet.util.Maths
    import cc.mallet.types.Dirichlet

    val uni = 1.0 / m.length.toDouble

    def lnorm(alpha : Double) = {
      var norm = (0.0 /: m)((a,b) => a + Dirichlet.logGamma(b+alpha)) 
      norm -= Dirichlet.logGamma((0.0 /: m)(_ + _ + alpha)) 
      norm
    }

    lnorm(10.0) / lnorm(.001)
  }

  def ig(m : Array[Int], tot : Array[Int]) = {

    val tt = (0.0 /: tot)(_ + _)
    val f = (0.0 /: m)(_ + _)
      
    val pU = f/tt

    var igain = pU * (0.0 /: m)((a,b) => {
      val p = b / f
      a + p * math.log(p)
    })

    igain += (1-pU) * (0.0 /: (m zip tot))((a,b) => {
      val p = (b._2 - b._1) / (tt - f)
      a + p * math.log(p)
    })
    
    igain
  }

  def chi2(m : Array[Int], p : Array[Double]) = {
    
    val n = (0 /: m)(_ + _)
    val av = p.map(_ * n)

    (0.0 /: 0.until(m.length))((a,b) => {
      a + math.pow(m(b).toDouble - av(b),2)/av(b)
    })
  }

  def main(args : Array[String]) = {
/**
    val a1 = Array(100,10,10,10)
    val a2 = Array(1000,1000,1000,900)
    val a3 = Array(1,2,20,0)

    println(pUniform(a1))
    println(pUniform(a2))
    println(pUniform(a3))

  }

  def poo() = {
*/
    val st  = new CFGSymbolTable()

    val fs = Array("/home/chonger/data/ICLE/icle_u.xml",
                   "/home/chonger/data/ICC/xml/icci_u.xml",
  //                 "/home/chonger/data/FCE/fce_u.xml",
                   "/home/chonger/data/Lang8/L8_u.xml")

    val ts = Array("/home/chonger/data/ICLE/icle-u-tsg.txt",
                   "/home/chonger/data/ICC/xml/icci-u-tsg.txt",
//                   "/home/chonger/data/FCE/fce-tsg.txt",
                 "/home/chonger/data/Lang8/L8-u-tsg.txt")

  val tdox = XMLDoc.read("/home/chonger/data/FCE/fce_u.xml",st)

    val dox = fs.map(x => XMLDoc.read(x,st))

    val grammar = {
      val g = new HashSet[ParseTree]()

      ts.foreach(t => {
        g ++= PTSG.read(t,st).rules.toList.flatMap(_.map(_._1))
      })

      val cfgs = TreeTools.cfgSet(dox.flatMap(_.flatMap(_.text).toList).toList).toList
      g ++= cfgs
      println("G created : Size = " + g.size )
      g.toList
    }

    //val cfgs = TreeTools.cfgSet(dox.flatMap(_.flatMap(_.text).toList).toList).toList                                                          

    val labels = (new HashSet[String] ++ dox.flatMap(d => d.map(_.getMeta("goldLabel")))).toArray

    val cod = new Compacter(grammar)
    val mdox = dox.map(_.map(d => {
      val tz = new HashSet[ParseTree]()
      val s = d.text(0)
      s.nonterminals.foreach(n => tz ++= cod.findOverlays(n).map(_._1))
      (d,tz)
    }))

    //look at language correlation
    val langP = grammar.flatMap(r => {
      
      val hasL = new HashMap[String,Int]()
      val hasD = new HashMap[Int,Int]()

      labels.foreach(l => {
        hasL(l) = 0
      })
      0.until(dox.length).foreach(i => {
        hasD(i) = 0
      })

      var tot = 0

      var ttt = List[(String,ParseTree)]()

      0.until(dox.length).foreach(i => {
        val md = mdox(i)
        md.foreach({
          case (d,tz) => {
            val l = d.getMeta("goldLabel")
            if(tz contains r) {
              ttt ::= (l,d.text(0))
              tot += 1
              hasL(l) = hasL.getOrElse(l,0) + 1
              hasD(i) = hasD.getOrElse(i,0) + 1
            }
          }
        })
      })
  
//      val l = chi2(hasL.iterator.map(_._2).toArray,Array(.25,.25,.25,.25))
  //    val d = chi2(hasD.iterator.map(_._2).toArray,Array(.277,.277,.1666,.277))
      val l = chi2(hasL.iterator.map(_._2).toArray,Array(.25,.25,.25,.25))
      val d = chi2(hasD.iterator.map(_._2).toArray,Array(.33,.33,.33))
      val ign = ig(hasL.iterator.map(_._2).toArray,dox.map(_.length))

      if(tot >= 20)
        List((r,l,hasL,d,hasD,ttt,ign))
      else
        Nil
    }).toArray

    val cutoff = 2.37
/**
    langP.sortWith(_._2 > _._2).foreach({
      case (tree,c,m,c1,m1) => {
        println(tree.fString(st))
        println(c + " - " + m.iterator.toArray.mkString(" "))
        println(c1 + " - " + m1.iterator.toArray.mkString(" "))
      }
    })
*/
    //x2 for language rejects the hypothesis that language is independent with p<.05
    //x2 for distance accepts the null hypothesis that we are data set independant
    val cuts = Array(1.39,2.41,3.22,4.60,5.99,9.21,13.82)
    //val ok = langP.filter(x => (x._2 >= 5.99) && (x._4 < 5.99))  
    val igSort = langP.sortWith(_._7 > _._7)
    val edox = dox.flatMap(x => x)
    val a1 = acc(st,edox,tdox,grammar.toList)

    val res = cuts.flatMap(c1 => {
      cuts.map(c2 => {
        println(c1 + "," + c2)
      val ok = langP.filter(x => (x._2 >= c1) && (x._4 < c2))  
      val okT = ok.map(_._1).toList
      println("Got " + ok.length)
      val igOK = igSort.slice(0,ok.length).map(_._1).toList
/**
    ok.foreach({
      case (tree,c,m,c1,m1,ttt,ign) => {
        println(tree.fString(st))
        println(c + " - " + m.iterator.toArray.mkString(" "))
        println(c1 + " - " + m1.iterator.toArray.mkString(" "))
        /**
        ttt.slice(0,10).foreach({
          case (l,t) => {
            println(l + ": " + t.sentence(st))
          }
        })
        */
      }
    })
*/

      val a2 = acc(st,edox,tdox,okT.toList)
      val a3 = acc(st,edox,tdox,igOK.toList)

      println("CHI2  : " + a2)
      println("IGAIN : " + a3)
      
      (c1,c2,a2,a3)
    })}).toArray

    res.foreach(x => {
      println(x)
    })

    println("FULL : " + a1)
  }

  def acc(st : CFGSymbolTable, d1 : Array[XMLDoc[ParseTree]],d2 : Array[XMLDoc[ParseTree]],grammar : List[ParseTree]) = {
    val pcfgex = new TSGExtractor(grammar,st)
    val pcfgI1 = pcfgex.featsBySent(d1)
    val pcfgI2 = pcfgex.featsBySent(d2)
    val pcfgC = pcfgex.classifier(.1)
   
    val insts1 = pcfgC.makeInsts(pcfgI1)
    val insts2 = pcfgC.makeInsts(pcfgI2)

    pcfgC.eval(insts1,insts2)
  }
}
