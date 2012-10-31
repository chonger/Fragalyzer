import scala.collection.mutable.{HashSet,HashMap}
import cc.mallet.classify._
import cc.mallet.classify.evaluate._
import cc.mallet.pipe._
import cc.mallet.pipe.iterator._
import cc.mallet.types._
import multitool._

class MalletClass[A](val extractor : FExtractor[A], regularizer : Option[Double]) {

  def this(e : FExtractor[A]) = this(e,None)

  val inds = new HashMap[A,Int]()

  val pipe : Pipe = {
    val pipeList = new java.util.ArrayList[Pipe]()
    pipeList.add(new Target2Label())
    pipeList.add(new Csv2FeatureVector())
    new SerialPipes(pipeList)
  }

  def toInds(hs : HashSet[A]) = {
    hs.iterator.map(dd => {
      if(!inds.keySet.contains(dd))
        inds += dd -> inds.size
      inds(dd)
    }).toList
  }

  def makeInsts(data : List[(String,HashSet[A])]) = {
    val insts = new InstanceList(pipe)
    data.foreach({
      case (lbl,d) => {
        val fStr = toInds(d).toArray.map(a => a + ":1.0").mkString(" ") + " defallt:1.0"
        val inst = new Instance(fStr,lbl,"NAME!","SOURCE")
        insts.addThruPipe(inst)
      }
    })
    insts
  }

  def getTrainer() = {
    regularizer match {
      case Some(d) => new MaxEntTrainer(d)
      case None => new MaxEntTrainer()
    } 
  }

  def crossValSD(data : List[(String,List[HashSet[A]])]) = {

    val dA = data.toArray
    Shuffle(dA)

    val dox = dA.map({
      case (lbl,ds) => {
        (lbl,makeInsts(ds.map(x => (lbl,x))))
      }
    }).toList

    println(inds.size + " features")

    val nFold = 5
    val splitS = dox.length / nFold
    var avgAcc = 0.0

    0.until(nFold).foreach(i => {
      val tr = dox.slice(0,splitS * i) ::: dox.drop(splitS*(i+1))
      val ts = dox.slice(splitS*i,splitS*(i+1))
      println("nTrain = " + tr.length)
      println("nTest = " + ts.length)

      val tr_insts = new InstanceList(pipe)
      tr.foreach(t => {
        tr_insts.addAll(t._2)
      })

      println("TRAINING SET SIZE- " + tr_insts.size)
      val trainer = getTrainer()
      try {
        trainer.train(tr_insts)
      } catch {
        case e : Exception => {
          println(e)
        }
      }
      
      val classifier = trainer.getClassifier()
      
      val hardC = true

      var acc = 0.0
      var tot = 0.0
      ts.foreach({
        case (goldLbl, sents) => {
          val votes = new HashMap[String,Double]()
      
          0.until(sents.size).foreach(i => {
            val s = sents.get(i)
            val lblin = classifier.classify(s).getLabeling()
            //GenClassifier.rocW.write(lblin.getBestLabel().toString + "," + goldLbl + "," + lblin.getBestValue() + "\n")
            if(hardC) {
              val bestL = lblin.getBestLabel().toString()
              val e = votes.getOrElse(bestL,0.0) + 1
              votes += bestL -> e
            } else {
              val lbls : Array[Object] = lblin.getLabelAlphabet().toArray
              lbls.foreach({
                case ll : String => {
                  val k = lblin.getLabelAlphabet().lookupLabel(ll)
                  val sc : Double = lblin.value(k)
                  val e = votes.getOrElse(k.toString,0.0) + sc
                  votes += k.toString -> e
                }
              })
            }
          })

          //println("GOLD - " + goldLbl)
          //println(votes.iterator.toArray.mkString(" "))

          tot += 1.0
          if(goldLbl == votes.iterator.toArray.sortWith(_._2 > _._2)(0)._1)
            acc += 1.0
        }
      })
      
      acc = acc/tot

      println("ACC = " + acc)

      avgAcc += acc / nFold
    })

    avgAcc
  }


  def crosscheck(data1 : List[(String,HashSet[A])], data2 : List[(String,HashSet[A])]) = {

    val insts1 = makeInsts(data1)
    val insts2 = makeInsts(data2)

    (eval(insts1,insts2) + eval(insts2,insts1)) / 2.0
  }

  def eval(tr_insts : InstanceList, ts_insts : InstanceList) = {
    val trainer = getTrainer()
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

    acc
  }

  def crossVal(data : List[(String,HashSet[A])], trainPr : Double, nTests : Int) = {

    val insts = makeInsts(data)

    println("Working with " + inds.size + " features")

    var avgAcc = 0.0

    0.until(nTests).foreach(testI => {
      
      val splits = insts.split(new java.util.Random(),Array(trainPr,1-trainPr))

      val tr_insts = splits(0);
      val ts_insts = splits(1);
    
      val trainer = getTrainer()
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

      avgAcc += acc / nTests.toDouble
    })

    avgAcc
  }


  def extremes(data : List[(String,HashSet[A])],xmlF : String) = {

    val dox = XMLDoc.read(xmlF,extractor.st)

    val insts = makeInsts(data)

    val lookup = inds.iterator.toArray.sortWith(_._2 < _._2).map(_._1)
    
    val trainer = getTrainer()
    try {
      trainer.train(insts)
    } catch {
      case e : Exception => {
        println(e)
      }
    }
    
    val classifier = trainer.getClassifier()

    val nFs  = math.min(inds.size,10)
    val sw = new java.io.StringWriter()
    val pw = new java.io.PrintWriter(sw)
    classifier.printExtremeFeatures(pw,nFs)
    pw.flush()
    pw.close()
    val s = sw.toString

    val exFeats = s.split("\\n").map(l => {
      val x = l.trim.split("\\s")
      
      val lbl = x(3)
      
      val fs = x.drop(4).slice(0,nFs).flatMap(fstr => {
        val p = fstr.split(":")
        try {
          val f = lookup(p(0).toInt)
          val counts = new HashMap[String,Int]()
          var tot = 0
          data.foreach({
            case (lbl,d) => {
              if(d contains f) {
                val e = counts.getOrElse(lbl,0) + 1
                counts += lbl -> e
                  tot += 1
              }
            }
          })
          if(tot > 20) {
            List((f,p(1),counts.iterator.toList.sortWith(_._2 > _._2)))
          } else
            Nil
        } catch {
          case _ => {
            //println("!" + p.toArray.mkString(" "))
            Nil
          }
        }
      }).slice(0,10)

      (lbl,fs)
    })

    exFeats.foreach({
      case (lbl,fs) => {
        println("\n" + lbl)
        val ddd = dox.filter(_.getMeta("goldLabel") == lbl).flatMap(_.text)
        fs.foreach({
          case (f,s,c) => {
            println(extractor.show(f) + " " + s + " - " + c.map(x => x._1 + ":" + x._2).mkString(" "))
            extractor.find(f,ddd)
          }
        })
      }
    })
  }

}


