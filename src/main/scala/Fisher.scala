import multitool._
import enbuske._
import scala.collection.mutable.{HashSet,HashMap}
import cc.mallet.classify._
import cc.mallet.classify.evaluate._
import cc.mallet.pipe._
import cc.mallet.pipe.iterator._
import cc.mallet.types._


object Fisher {

  def main(args : Array[String]) : Unit = {
    val fb = "/home/chonger/data/ICLE/"
    val st = new CFGSymbolTable()
    val dox = XMLDoc.read(fb + "icle_bnp_unk.xml",st)    
    val cfgs = TreeTools.cfgSet(dox.flatMap(_.text).toList).toList

    Shuffle(dox)

    val folds = 5

    val fSize = dox.length / folds

    var totAcc = 0.0

    0.until(folds).foreach(f => {
      val train = dox.slice(0,fSize*f).toList ::: dox.drop(fSize*(f+1)).toList
      val test = dox.slice(fSize*f,fSize*(f+1))

      val fisher = new Fisher(train.toArray,st,cfgs)

      val acc = fisher.getSentAccuracy(test)

      println("ACC - " + acc)

      totAcc += acc
    })
    
    totAcc / folds.toDouble
  }

}

class Fisher(train : Array[XMLDoc[ParseTree]], st : CFGSymbolTable, cfgs : List[ParseTree]) {

  val tG = train.groupBy(_.getMeta("goldLabel"))
  val labels = tG.map(_._1).toArray
  val ptsgs = tG.map(_._2.flatMap(_.text)).map(x => PTSG.mlPCFG(st,x.toList,cfgs)).toArray

  val inds = new HashMap[ParseTree,Int]()
  cfgs.foreach(c => {
    inds += c -> inds.size
  })

  val pipe : Pipe = {
    val pipeList = new java.util.ArrayList[Pipe]()
    pipeList.add(new Target2Label())
    pipeList.add(new Csv2FeatureVector())
    new SerialPipes(pipeList)
  }

  val tr_insts = getInsts(train)

  var maxV = 0.0

  def getFisher(t : ParseTree) = {

    val divFac = 1.0//math.pow(10,-8)

    val scores = ptsgs.map(g => {
      val p = g.score(t)
      if(p == 0)
        throw new Exception()
      p
    })
    val clN = 1.0 / scores.length.toDouble

    val tot = (0.0 /: scores)(_ + _) * clN

    if(tot == 0)
      throw new Exception()

    val classF : List[(String,Double)] = (labels zip scores).map({
      case (l,s) => {
        (l,divFac*s/tot)
      } 
    }).toList

    val ruleF = t.nonterminals.groupBy(x => x).flatMap({
      case (n,ns) => {
        val pt = new ParseTree(n.rule.node())
        val count = ns.length.toDouble
        0.until(scores.length).map(i => {
          val sc = scores(i) * count / ptsgs(i).rules(n.symbol)(pt) * clN
          val v = sc / tot
          if(v > maxV) 
            maxV = v
          (labels(i)+"-"+inds(pt),divFac*sc/tot)
        })
      }
    }).toList

    classF ::: ruleF
  }

  def getInsts(dox : Array[XMLDoc[ParseTree]]) = {
    val insts = new InstanceList(pipe)
    dox.foreach(d => {
      val lbl = d.getMeta("goldLabel")
      d.text.foreach(t => {
        val fStr = getFisher(t).toArray.map(a => a._1 + ":" + a._2).mkString(" ") + " defallt:1.0"
        val inst = new Instance(fStr,lbl,"NAME!",t.pString(st))
        insts.addThruPipe(inst)
      })
    })
    insts
  }
  
  def getSentAccuracy(dox : Array[XMLDoc[ParseTree]]) = {

    import java.io._
    var bw = new BufferedWriter(new FileWriter("/home/chonger/data/ICLE/FISHER_TRAIN.txt"))

    val fInds = new HashMap[String,Int]()
    fInds += "ddd" -> 0
    fInds += "dddd" -> 1

    train.foreach(d => {
      val lbl = labels.indexOf(d.getMeta("goldLabel"))+1
      d.text.foreach(t => {
        val fStr = "1:1.0 " + getFisher(t).toArray.map(a => {
          (fInds.getOrElseUpdate(a._1,fInds.size),a._2)
        }).sortWith(_._1 < _._1).map(a => a._1 + ":" + a._2).mkString(" ") 
        bw.write(lbl +" " + fStr + "\n")
      })
    })
    bw.close()
    bw = new BufferedWriter(new FileWriter("/home/chonger/data/ICLE/FISHER_TEST.txt"))
    dox.foreach(d => {
      val lbl = labels.indexOf(d.getMeta("goldLabel"))+1
      d.text.foreach(t => {
        val fStr = "1:1.0 " + getFisher(t).toArray.map(a => {
          (fInds.getOrElseUpdate(a._1,fInds.size),a._2)
        }).sortWith(_._1 < _._1).map(a => a._1 + ":" + a._2).mkString(" ") 
        bw.write(lbl +" " + fStr + "\n")
      })
    })
    bw.close()
    
    throw new Exception()

    val ts_insts = getInsts(dox)                            

    var bestAcc = 0.0

    val accx = 0.until(10).map(x => {
      val trainer = new MaxEntTrainer(math.pow(10,x))
      try {
        trainer.train(tr_insts)
      } catch {
        case e : Exception => {
          println(e)
        }
      }
      val classifier = trainer.getClassifier()
      
      val trial = new Trial(classifier, ts_insts)
      val acc = trial.getAccuracy()
      
      println("Accuracy: " + acc)
      println(new ConfusionMatrix(trial))
      
      if(acc > bestAcc)
        bestAcc = acc
      acc
    }).toArray

    println(accx.mkString("\n") + "\n")

    bestAcc
  }


}
