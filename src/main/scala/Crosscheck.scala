import multitool._
import enbuske._
import java.io._

import scala.collection.mutable.{HashMap,HashSet}

/***
 *
  *    REG
  *
  *    BNP - D/38.8  G/39.7  S/41.7
  *
  * 
 DISCRIMINATIVE
ICLE - ICCI : 0.3880108893060089
ICLE - LANG8 : 0.36701767485822306
LANG8 - ICCI : 0.35062205166918303
GENERATIVE
ICLE - ICCI : 0.3972534676404823
ICLE - LANG8 : 0.3429334593572779
LANG8 - ICCI : 0.3325936587820835
  *
  *
  * ((0.4273,0.3986),(0.30816666666666664,0.3542),(0.33350774382586856,0.3559),(0.28983333333333333,0.3499),(0.30692758476349935,0.3754),(0.3170782754290498,0.3335))
  ((0.4362,0.4104),(0.31633333333333336,0.351),(0.33591460862285477,0.3542),(0.29783333333333334,0.3692),(0.3132063624947677,0.3696),(0.3195897865215571,0.315))

 *   DISCRIMIANTE
  * Vector((0,1,0.1,0.4273), (0,2,1.0,0.30766666666666664), (0,3,1.0,0.3114), (1,0,1.0,0.392), (1,2,0.1,0.28983333333333333), (1,3,0.1,0.3067), (2,0,0.01,0.3532), (2,1,0.1,0.3499), (2,3,0.01,0.3039), (3,0,0.1,0.351), (3,1,0.1,0.3652), (3,2,0.1,0.32816666666666666))

* D-TSG
*Vector((0,1,0.1,0.4375), (0,2,0.1,0.31616666666666665), (0,3,0.1,0.33874005860192546), (1,0,1.0,0.3887), (1,2,0.1,0.30133333333333334), (1,3,0.01,0.3075554625366262), (2,0,0.01,0.3621), (2,1,0.01,0.374), (2,3,0.01,0.3247174550020929), (3,0,0.01,0.3613), (3,1,0.01,0.3765), (3,2,0.01,0.32133333333333336))

  * GEN-PCFG
  * Vector((0,1,0.1,0.4364), (0,2,10.0,0.2905), (0,3,10.0,0.2962), (1,0,0.1,0.4002), (1,2,1.0,0.2931666666666667), (1,3,1.0,0.2825), (2,0,1.0,0.3225), (2,1,10.0,0.3149), (2,3,10.0,0.2819), (3,0,0.01,0.315), (3,1,1.0,0.3118), (3,2,1.0,0.309))

*   GEN-TSG
 * Vector((0,1,1.0,0.4431), (0,2,10.0,0.29283333333333333), (0,3,10.0,0.3015906236919213), (1,0,0.01,0.3777), (1,2,1.0,0.30133333333333334), (1,3,1.0,0.29280033486814566), (2,0,1.0,0.3246), (2,1,1.0,0.3406), (2,3,1.0,0.28840519045625784), (3,0,1.0,0.3211), (3,1,1.0,0.3269), (3,2,1.0,0.2916666666666667))

  * 
  */ 


object GenSemi {

  val data = "/home/chonger/data/ICLE/icle_unk.xml"

  def main(args : Array[String]) = {
    val st = new CFGSymbolTable()

    val dox = XMLDoc.read(data,st)

    val items = dox.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toArray

    println("AVGACC : " + Crosscheck.crossVal(items,st))
  }
}

object MakeROC {
  
  val data1 = "/home/chonger/data/ICLE/icle_x.xml"
  val data2 = "/home/chonger/data/ICC/xml/icci_x.xml"

  val rocRaw = "/home/chonger/dump/roc-disc-raw.txt"
  val rocOut = "/home/chonger/dump/roc-disc.txt"

  def main(args : Array[String]) = {

    val bw = new BufferedWriter(new FileWriter(rocRaw))

    danalyze(data1,data2).foreach({
      case (gold,res) => {

        println(gold + " - " + res)
        
        var sc = 0.0
        val tot = (0.0 /: res)((a,b) => {
          b match {
            case (l,s) => {
              if(l == gold)
                sc = s
              a + s
            }
          }
        })
        var best = (0.0,"fail")
        res.foreach({
          case (l,s) => {
            if(s > best._1)
              best = (s,l)
          }
        })
        sc = sc/tot
        //bw.write(best._2 + "," + gold + "," + sc + "\n")
        bw.write(best._2 + "," + gold + "," + res.map(x => x._1 + "#" + x._2).mkString("|") + "\n")
      }
    })

    bw.close()

    //ROC(rocRaw,rocOut)

  }

  def danalyze(train : String, test : String) : List[(String,List[(String,Double)])] = {
    val pcfgex = new PCFGExtractor()
    val pcfgI1 = pcfgex.featsBySent(train)
    val pcfgI2 = pcfgex.featsBySent(test)
    val pcfgC = pcfgex.classifier(.1)

    val insts1 = pcfgC.makeInsts(pcfgI1)
    val insts2 = pcfgC.makeInsts(pcfgI2)

    pcfgC.analyze(insts1,insts2)
  }

  def ganalyze(train : String, test : String) : List[(String,List[(String,Double)])] = {

    val st = new CFGSymbolTable()

    val dox1 = XMLDoc.read(train,st)
    val dox2 = XMLDoc.read(test,st)

    val cfgs = TreeTools.cfgSet((dox1.toList ::: dox2.toList).flatMap(_.text).toList).toList

    val items1 = dox1.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList
    val items2 = dox2.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList

    GenClassifier.analyze(items1,items2,st,cfgs)

  }

}

object Crosscheck {

  val data1 = "/home/chonger/data/ICLE/icle_u.xml"
  val data2 = "/home/chonger/data/ICC/xml/icci_u.xml"
  val data3 = "/home/chonger/data/FCE/fce_u.xml"
  val data4 = "/home/chonger/data/Lang8/L8_u.xml"
  val tsg1 = "/home/chonger/data/ICLE/icle-tsg.txt"
  val tsg2 = "/home/chonger/data/ICC/xml/icci-tsg.txt"
//  val data3 = "/home/chonger/data/Lang8/L8-x.xml"

  def main(args : Array[String]) = {

    val d1 = gen(Array(data1,data2,data3,data4))
    println(d1)
    //val d1T = discTSG(data1,data2,tsg1,tsg2)
  //  val d2 = disc(data2,data3)
   // val d3 = disc(data1,data3)

    //val g1 = gen(data1,data2)
//    val g1T = genTSG(data1,data2,tsg1,tsg2)
    //val g2 = gen(data2,data3)
    //val g3 = gen(data1,data3)

//    val s1 = semi(data1,data2)
//    val s2 = semi(data2,data3)
//    val s3 = semi(data1,data3)

  
    println("DISCRIMINATIVE")
    //println("PCFG : ICLE - ICCI  : " + d1)
    //println("TSG  : ICLE - ICCI  : " + d1T)
  //  println("ICCI - LANG8 : " + d2)
    //println("ICLE - LANG8 : " + d3)

    println("GENERATIVE")
    //println("PCFG : ICLE - ICCI  : " + g1)
  //  println("TSG  : ICLE - ICCI  : " + g1T)
    //println("ICCI - LANG8 : " + g2)
    //println("ICLE - LANG8 : " + g3)

    //println("SEMI")
  //  println("ICLE - ICCI  : " + s1)
//    println("ICCI - LANG8 : " + s2)
  //  println("ICLE - LANG8 : " + s3)

  }

  def disc(fs : Array[String]) = {

    val pcfgex = new PCFGExtractor()
    val items = fs.map(f => pcfgex.featsBySent(f))

    println("READY")

    val nItems = fs.length
    0.until(nItems).flatMap(x => {
      0.until(nItems).flatMap(y => {
        if(x != y) {
          val dev = 0.until(nItems).filter(z => z != x && z != y)
          val ch = 0.until(5).map(z => {
            val sm = math.pow(10,z-2)
            val pcfgC = pcfgex.classifier(sm)
            val e = (0.0 /: dev)((a,b) => a + pcfgC.eval(pcfgC.makeInsts(items(x)),pcfgC.makeInsts(items(b))))
            (e,sm)
          }).toArray
          val useS = ch.sortWith(_._1 > _._1)(0)._2
          println("Chose " + useS)
          val pcfgC = pcfgex.classifier(useS)
          val r = pcfgC.eval(pcfgC.makeInsts(items(x)),pcfgC.makeInsts(items(y)))
          List((x,y,useS,r))
        } else {
          Nil
        }
      })
    })
  }

  def discTSG(d1 : String, d2 : String, tsg1 : String, tsg2 : String) = {

    val st = new CFGSymbolTable()

    val grammar = new HashSet[ParseTree]()
    grammar ++= PTSG.read(tsg1,st).rules.toList.flatMap(_.map(_._1))
    grammar ++= PTSG.read(tsg2,st).rules.toList.flatMap(_.map(_._1))

    println("Grammar created")

    val pcfgex = new TSGExtractor(grammar.toList,st)
    val pcfgI1 = pcfgex.featsBySent(d1)
    val pcfgI2 = pcfgex.featsBySent(d2)
    val pcfgC = pcfgex.classifier(.1)
    /**
    val cc = pcfgC.crosscheck(pcfgI1,pcfgI2)
    println("CROSS : " + cc)
    cc
*/
    val insts1 = pcfgC.makeInsts(pcfgI1)
    val insts2 = pcfgC.makeInsts(pcfgI2)

    (pcfgC.eval(insts1,insts2),pcfgC.eval(insts2,insts1))
  }

  def gen(fs : Array[String]) = {

    val st = new CFGSymbolTable()

    val dox = fs.map(x => XMLDoc.read(x,st))

    val cfgs = TreeTools.cfgSet(dox.flatMap(_.flatMap(_.text).toList).toList).toList

    val items = dox.map(ds => ds.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList)

    //(GenClassifier.crosscheck(items1,items2,st,cfgs),GenClassifier.crosscheck(items2,items1,st,cfgs))

    println("READY")

    val nItems = fs.length

    0.until(nItems).flatMap(x => {
      0.until(nItems).flatMap(y => {
        if(x != y) {
          val dev = 0.until(nItems).filter(z => z != x && z != y)
          val ch = 0.until(5).map(z => {
            val sm = math.pow(10,z-2)
            val e = (0.0 /: dev)((a,b) => a + GenClassifier.crosscheck(items(x),items(b),st,cfgs,sm))
            (e,sm)
          }).toArray
          val useS = ch.sortWith(_._1 > _._1)(0)._2
          println("Chose " + useS)
          val r = GenClassifier.crosscheck(items(x),items(y),st,cfgs,useS)
          List((x,y,useS,r))
        } else {
          Nil
        }
      })
    })

  }

  def genTSG(d1 : String, d2 : String, tsg1 : String, tsg2 : String) = {

    val st = new CFGSymbolTable()

    val grammar = new HashSet[ParseTree]()
    grammar ++= PTSG.read(tsg1,st).rules.toList.flatMap(_.map(_._1))
    grammar ++= PTSG.read(tsg2,st).rules.toList.flatMap(_.map(_._1))

    val dox1 = XMLDoc.read(d1,st)
    val dox2 = XMLDoc.read(d2,st)

    val items1 = dox1.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList
    val items2 = dox2.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList

    (GenClassifier.crosscheckTSG(items1,items2,st,grammar.toList),GenClassifier.crosscheckTSG(items2,items1,st,grammar.toList))

  }

  def semi(d1 : String, d2 : String) = {

    val st = new CFGSymbolTable()

    val dox1 = XMLDoc.read(d1,st)
    val dox2 = XMLDoc.read(d2,st)

    val cfgs = TreeTools.cfgSet((dox1.toList ::: dox2.toList).flatMap(_.text).toList).toList

    //list[string,parsetree]
    val items1 = dox1.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList
    val items2 = dox2.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList

    (check(items1,items2,st,cfgs),check(items2,items1,st,cfgs))

  }

  def semiTSG(d1 : String, d2 : String, tsg1 : String, tsg2 : String) = {

    val st = new CFGSymbolTable()

    val dox1 = XMLDoc.read(d1,st)
    val dox2 = XMLDoc.read(d2,st)

    val grammar = new HashSet[ParseTree]()
    grammar ++= PTSG.read(tsg1,st).rules.toList.flatMap(_.map(_._1))
    grammar ++= PTSG.read(tsg2,st).rules.toList.flatMap(_.map(_._1))

    //list[string,parsetree]
    val items1 = dox1.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList
    val items2 = dox2.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList

    (check(items1,items2,st,grammar.toList),check(items2,items1,st,grammar.toList))

  }

  def crossVal(items : Array[(String,ParseTree)], st : CFGSymbolTable) = {

    val folds = 3
    val split = .2
    val fSize : Int = (items.length * split).toInt

    val cfgs = TreeTools.cfgSet(items.map(_._2).toList).toList

    var avg = 0.0
    0.until(folds).foreach(f => {
      Shuffle(items)

      val train = items.slice(0,fSize).toList
      val test = items.drop(fSize).toList

      val tG = train.groupBy(_._1)

      val labels = tG.map(_._1).toArray

      val ptsgs = tG.map(x => PTSG.mlPCFG(st,x._2.map(_._2).toList,cfgs)).toArray

      println("CLASSIFYING")

      val gc = new GenClassifier(ptsgs,labels)

      val acc = gc.getAccuracy(test.toList)

      println("ACC - " + acc)


      val cc = check(train,test,st,cfgs) 
      println("AAAACCCC:" + cc)
      avg += cc / folds.toDouble
    })
    avg
  }

  def check(labeled : List[(String,ParseTree)], unlabeled : List[(String,ParseTree)],st : CFGSymbolTable, grammar : List[ParseTree]) = {

    val data = labeled.groupBy(_._1)

    val labels = data.map(_._1).toArray
    val nLabels = labels.length

    println("init PCFGS")

    var ptsgs = {
      val hm = new HashMap[ParseTree,Double]()
      hm ++= grammar.map(x => (x,.1))
      val base = new PTSG(st,hm)
      data.map(y => PTSG.emPTSG(st,base,y._2.map(_._2),100,1)).toArray //initialize with the emTSG for each labeled section
    }

    def preproc(d : List[(String,ParseTree)]) = {
      d.map({
        case(s,t) => {
          (labels.indexOf(s),t,ptsgs(0).getOverlays(t))
        }
      })
    }

    val ldata = preproc(labeled)
    val udata = preproc(unlabeled)

    var accu = 0.0

    var converged = false
    var lastLL = 0.0

    0.until(100).foreach(i => {
      if(!converged)
        accu = emIter()
    })

    def emIter() = {
    
      val exC = Array.tabulate(nLabels)(x => new HashMap[ParseTree,Double]())

      var lprob = 0.0

      println("get labeled expectations")

      ldata.par.foreach({
        case (lInd,t,oles) => {
          val ptsg = ptsgs(lInd)
          val (eee,norm) = ptsg.getEx(t,oles)
          synchronized {
            lprob += math.log(norm) - math.log(ptsg.PT_BOOST)*t.preterminals.length
            val m = exC(lInd)
            eee.foreach({
              case (t,p) => {
                m(t) = m.getOrElse(t,0.0) + p
              }
            })
          }
        }
      })
      
      println("get unlabled expectations")

      //var used = 0
      var acc = 0.0
      var tot = udata.length.toDouble

      udata.par.foreach({
        case (lInd,t,oles) => {
          val exes = ptsgs.map(_.getEx(t,oles))
          val norms = exes.map(_._2)
          var best = (0.0,-1)
          val tnorm = (0.0 /: 0.until(nLabels))((a,b) => {
            if(norms(b) > best._1)
              best = (norms(b),b)
            a + norms(b)
          })  
          val proportions = norms.map(_/tnorm)
          synchronized {
            if(best._2 == lInd)
              acc += 1.0
            lprob += math.log(tnorm) - math.log(ptsgs(0).PT_BOOST)*t.preterminals.length - math.log(nLabels)
            0.until(nLabels).foreach(i => {
              val prop = proportions(i)
              val eee = exes(i)._1
              val m = exC(i)
              eee.foreach({
                case (t,p) => {
                  m(t) = m.getOrElse(t,0.0) + p*prop
                }
              })
            })
          }
        }
      })

      //println("USED : " + used + " out of " + unlabeled.length)

      val smooth = .00001

      //estimate new pcfgs

      ptsgs = exC.map(ex => {
        val rules = new HashMap[ParseTree,Double]()
        val norms = new HashMap[Int,Double]()
        grammar.foreach(c => {
          val count = ex.getOrElse(c,0.0) + smooth
          rules += c -> count
          val sym = c.root.symbol
          norms(sym) = norms.getOrElse(sym,0.0) + count
        })

        rules.foreach({
          case (r,d) => rules(r) = d/norms(r.root.symbol)
        })
        new PTSG(st,rules)
      })


      if(lastLL != 0) {
        if(lprob - lastLL < .0001) {
          println("CONVERGED")
          converged = true
        }
      }
      lastLL = lprob

      println("LOGPROB : " + lprob)
      println("Curr ACC = " + acc/tot)
      acc/tot
    }

    accu
  }

  def checkOLD(labeled : List[(String,ParseTree)], unlabeled : List[(String,ParseTree)],st : CFGSymbolTable, cfgs : List[ParseTree]) = {

    val data = labeled.groupBy(_._1)

    val labels = data.map(_._1).toArray
    val nLabels = labels.length
    
    println("add labeled counts")

    val labeledC = data.map(_._2).map(x => {
      val counts = new HashMap[ParseTree,Double]()

      x.foreach({
        case (l,tree) => {
          tree.nonterminals.foreach(n => {
            val rule = new ParseTree(n.rule.node())
            counts(rule) = counts.getOrElse(rule,0.0) + 1.0
          })
        }
      })
      counts
    }).toArray

    val lll = labeled.groupBy(_._1)

    println("init PCFGS")

    var ptsgs = labels.map(y => PTSG.mlPCFG(st,lll(y).map(_._2),cfgs)) //initialize with the mlPCFG for the labeled section

    var accu = 0.0

    val dirichletP = .00001

    var converged = false
    var lastLL = 0.0

    0.until(100).foreach(i => {
      if(!converged)
        accu = emIter()
    })

    def emIter() = {
      println("EM")
      var acc = 0.0
      var tot = 0.0

      val exC = Array.tabulate(nLabels)(x => new HashMap[ParseTree,Double]())

      var lprob = 0.0

      (data.map(_._2).zip(ptsgs)).foreach({
        case (tz,pcfg) => {
          tz.foreach({
            case (l,tree) => {
              lprob += math.log(pcfg.score(tree)) - math.log(pcfg.PT_BOOST)*tree.preterminals.length
            }
          })
        }
      })

      0.until(nLabels).foreach(i => {
        labeledC(i).foreach({
          case(t,d) => exC(i) += t->d
        })
      })
      
      println("get unlabled expectations")

      var used = 0

      unlabeled.par.foreach({
        case (l,t) => {
          val probs = ptsgs.map(g => g.score(t))// + dirichletP * math.pow(g.PT_BOOST,t.preterminals.length))

          val totalP = (0.0 /: probs)(_ + _)
          synchronized {
            lprob += math.log(totalP) - math.log(ptsgs(0).PT_BOOST)*t.preterminals.length - math.log(nLabels)
          }
          
          val best = ((0.0,-1)  /: 0.until(nLabels))((a,b) => {
            if(probs(b) > a._1)
              (probs(b),b)
            else
              a
          })

          if(best._2 == -1)
            println(probs.mkString(" "))

          synchronized {
            if(labels(best._2) == l)
              acc += 1
            tot += 1
          }

          val hard = false

          if(best._1/totalP > 0.0) {
            synchronized {
              used += 1
            }
            t.nonterminals.foreach(n => {
              val rule = new ParseTree(n.rule.node())
              synchronized {
                if(hard) {
                  val m = exC(best._2)
                  m(rule) = m.getOrElse(rule,0.0) + probs(best._2)/totalP
                } else {
                  0.until(nLabels).foreach(lI => {
                    val m = exC(lI)
                    m(rule) = m.getOrElse(rule,0.0) + probs(lI)/totalP
                  })
                }
              }
            })
          }
        }
      })

      println("USED : " + used + " out of " + unlabeled.length)

      val smooth = .00001

      //estimate new pcfgs

      ptsgs = exC.map(ex => {
        val rules = new HashMap[ParseTree,Double]()
        val norms = new HashMap[Int,Double]()
        cfgs.foreach(c => {
          val count = ex.getOrElse(c,0.0) + smooth
          rules += c -> count
          val sym = c.root.symbol
          norms(sym) = norms.getOrElse(sym,0.0) + count
        })

        rules.foreach({
          case (r,d) => rules(r) = d/norms(r.root.symbol)
        })
        new PTSG(st,rules)
      })


      if(lastLL != 0) {
        if(lprob - lastLL < .0001) {
          println("CONVERGED")
          converged = true
        }
      }
      lastLL = lprob

      println("LOGPROB : " + lprob)
      println("Curr ACC = " + acc/tot)
      acc/tot
    }

    accu
  }
}
