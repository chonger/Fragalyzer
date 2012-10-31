import multitool._
import enbuske._

import scala.collection.mutable.HashMap

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

object Crosscheck {

  val data1 = "/home/chonger/data/ICLE/icle_x.xml"
  val data2 = "/home/chonger/data/ICC/xml/icci_x.xml"
  val data3 = "/home/chonger/data/Lang8/L8-x.xml"

  def main(args : Array[String]) = {

    val d1 = disc(data1,data2)
    val d2 = disc(data2,data3)
    val d3 = disc(data1,data3)

    val g1 = gen(data1,data2)
    val g2 = gen(data2,data3)
    val g3 = gen(data1,data3)
/**
    val s1 = semi(data1,data2)
    val s2 = semi(data2,data3)
    val s3 = semi(data1,data3)
*/
  
    println("DISCRIMINATIVE")
    println("ICLE - ICCI  : " + d1)
    println("ICCI - LANG8 : " + d2)
    println("ICLE - LANG8 : " + d3)

    println("GENERATIVE")
    println("ICLE - ICCI  : " + g1)
    println("ICCI - LANG8 : " + g2)
    println("ICLE - LANG8 : " + g3)
/**
    println("SEMI")
    println("ICLE - ICCI  : " + s1)
    println("ICCI - LANG8 : " + s2)
    println("ICLE - LANG8 : " + s3)
*/
  }

  def disc(d1 : String, d2 : String) = {

    val pcfgex = new PCFGExtractor()
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

  def gen(d1 : String, d2 : String) = {

    val st = new CFGSymbolTable()

    val dox1 = XMLDoc.read(d1,st)
    val dox2 = XMLDoc.read(d2,st)

    val cfgs = TreeTools.cfgSet((dox1.toList ::: dox2.toList).flatMap(_.text).toList).toList

    val items1 = dox1.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList
    val items2 = dox2.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList

    (GenClassifier.crosscheck(items1,items2,st,cfgs),GenClassifier.crosscheck(items2,items1,st,cfgs))
/**
    val cc = (GenClassifier.crosscheck(items1,items2,st,cfgs) + GenClassifier.crosscheck(items2,items1,st,cfgs))/2.0

    println("CROSS : " + cc)

    cc
*/
  }

  def semi(d1 : String, d2 : String) = {

    val st = new CFGSymbolTable()

    val dox1 = XMLDoc.read(d1,st)
    val dox2 = XMLDoc.read(d2,st)

    val cfgs = TreeTools.cfgSet((dox1.toList ::: dox2.toList).flatMap(_.text).toList).toList

    //list[string,parsetree]
    val items1 = dox1.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList
    val items2 = dox2.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList

    (check(items2,items1,st,cfgs) + check(items1,items2,st,cfgs)) / 2.0

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

  def check(labeled : List[(String,ParseTree)], unlabeled : List[(String,ParseTree)],st : CFGSymbolTable, cfgs : List[ParseTree]) = {

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

    0.until(100).foreach(i => {
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

          lprob += math.log(totalP) - math.log(ptsgs(0).PT_BOOST)*t.preterminals.length - math.log(nLabels)

          val best = ((0.0,-1)  /: 0.until(nLabels))((a,b) => {
            if(probs(b) > a._1)
              (probs(b),b)
            else
              a
          })

          if(best._2 == -1)
            println(probs.mkString(" "))

          if(labels(best._2) == l)
            acc += 1
          tot += 1

          val hard = false

          if(best._1/totalP > 0.0) {
            used += 1
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

      println("LOGPROB : " + lprob)
      println("Curr ACC = " + acc/tot)
      acc/tot
    }

    accu
  }
}
