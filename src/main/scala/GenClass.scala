import multitool._
import enbuske._

object GenClassifier {

  import java.io._
  val rocFile = "/home/chonger/data/ICLE/roc.txt"

  //val rocW = new BufferedWriter(new FileWriter(rocFile))
  
  def main(args : Array[String]) = {

    val fb = "/home/chonger/data/ICLE/"
/**    
    val es = ESampler.continue(fb + "icle_bnp_unk.xml",fb + "bnp-sampled.txt")
    val ptsg = es.getPTSGs()
    ptsg(0).write(fb + "bnp-grammar.xml")
  */

    val st = new CFGSymbolTable()
    val dox1 = XMLDoc.read(fb + "icle_unk.xml",st)    
    val dox2 = XMLDoc.read("/home/chonger/data/ICC/xml/bparsed.xml",st)    

    val cfgs = TreeTools.cfgSet((dox1.toList ::: dox2.toList).flatMap(_.text).toList).toList

    val items1 = dox1.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList
    val items2 = dox2.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x))).toList

    println("CROSS : " + (crosscheck(items1,items2,st,cfgs) + crosscheck(items2,items1,st,cfgs))/2.0)

/**
    dox.groupBy(_.getMeta("goldLabel")).foreach({
      case (l,ds) => println(l + " -> " + (0 /: ds)(_ + _.text.length))
    })
*/
    //println("XVAL : " + pcfgXVAL(dox,st,TreeTools.cfgSet(dox.flatMap(_.text).toList).toList))

    //val ptsg = PTSG.read(fb + "reg-grammar.txt",st)    
    //println("XVAL : " + tsgXVAL(dox,st,ptsg))

  //  println("XVAL : " + enbuskeXVAL(dox,st))
    
    //rocW.close()
  }

  def tsgXVAL(dox : Array[XMLDoc[ParseTree]], st : CFGSymbolTable, grammar : List[ParseTree]) : Double = {
    import scala.collection.mutable.HashMap
    val hm = new HashMap[ParseTree,Double]()
    hm ++= grammar.map(x => (x,.1))
    val base = new PTSG(st,hm)
    tsgXVAL(dox,st,base)
  }

  def tsgXVAL(dox : Array[XMLDoc[ParseTree]], st : CFGSymbolTable, base : PTSG) : Double = {
    genXVAL(dox,st,x => x.map(y => PTSG.emPTSG(st,base,y,20)))
  }

  def pcfgXVAL(dox : Array[XMLDoc[ParseTree]], st : CFGSymbolTable, cfgs : List[ParseTree]) = {
    genXVAL(dox,st,x => x.map(y => PTSG.mlPCFG(st,y,cfgs)))
  }
  
  def enbuskeXVAL(dox : Array[XMLDoc[ParseTree]], st : CFGSymbolTable) : Double = {

    val nIters = 10

    def proc(data : Array[List[ParseTree]]) : Array[PTSG] = {

      val typeA = 0.until(data.length).map(_.toString).toArray
      val dox = 0.until(data.length).flatMap(dI => {
        data(dI).map(x => new XMLDoc[ParseTree](List(x),Array(("goldLabel",dI.toString()))))
      }).toArray

      val nT = typeA.length
      
      val theta = Array.tabulate(nT)(x => Array.tabulate(nT)(y => {
        if(x == y)
          100000
        else
          0.00001
      }))
      val alphas = Array.tabulate(nT)(x => 100.0)
      val gamma = 1000.0
      
      val pcfg = new PCFG(st,dox)
      val esampler = new ESampler(dox,st,pcfg,typeA,alphas,gamma,theta)
      esampler.doSampling(nIters,null)
      esampler.getPTSGs()

    }

    genXVAL(dox,st,proc)
  }


  def crosscheck(train : List[(String,ParseTree)], test : List[(String,ParseTree)], st : CFGSymbolTable, cfgs : List[ParseTree]) = {
    val tG = train.groupBy(_._1)

    def proc(x : Array[List[ParseTree]]) = x.map(y => PTSG.mlPCFG(st,y,cfgs))

    val labels = tG.map(_._1).toArray
    
    val ptsgs = proc(tG.map(_._2.map(_._2).toList).toArray)
    
    println("CLASSIFYING")
    
    val gc = new GenClassifier(ptsgs,labels)
    
    val acc = gc.getAccuracy(test.toList)
    
    println("ACC - " + acc)
    
    acc
  }

  def genXVAL(dox : Array[XMLDoc[ParseTree]], st : CFGSymbolTable, proc : (Array[List[ParseTree]]) => Array[PTSG]) = {

    val folds = 10

    val items = dox.flatMap(d => d.text.map(x => (d.getMeta("goldLabel"),x)))

    val fSize = (items.length * .2).toInt

    println("TOTAL OF " + items.length)
    println("USING " + fSize  + " for training")

    var totAcc = 0.0

    0.until(folds).foreach(f => {

      Shuffle(items)

      val train = items.slice(0,fSize).toList
      val test = items.drop(fSize)

      val tG = train.groupBy(_._1)

      val labels = tG.map(_._1).toArray

      val ptsgs = proc(tG.map(_._2.map(_._2).toList).toArray)

      println("CLASSIFYING")

      val gc = new GenClassifier(ptsgs,labels)

      val acc = gc.getAccuracy(test.toList)

      println("ACC - " + acc)

      totAcc += acc
    })
    
    totAcc / folds.toDouble
  }

}

class GenClassifier(val ptsgs : Array[PTSG], val labels : Array[String]) {

  def scoreDoc(d : XMLDoc[ParseTree],ptsg : PTSG) = (0.0 /: d.text)((x,y) => x + math.log(ptsg.score(y)))

  def classify(d : XMLDoc[ParseTree]) = {
    ((0,scoreDoc(d,ptsgs(0))) /: 1.until(ptsgs.length))((x,y) => {
      val sc = scoreDoc(d,ptsgs(y))
      if(sc > x._2)
        (y,sc)
      else
        x
    })
  } 

  def classify(t : ParseTree) = {

    val indSc = 0.until(ptsgs.length).map(x => ptsgs(x).score(t))
    
    val tot = (0.0 /: indSc)(_ + _)

    val (i,s) = ((0,indSc(0)) /: 1.until(ptsgs.length))((x,y) => {
      val sc = indSc(y) 
      if(sc > x._2)
        (y,sc)
      else
        x
    })
    (i,s/tot)
  }

  def getAccuracy(items : List[(String,ParseTree)]) = {
    var correct = 0.0
    var tot = 0.0

    items.foreach({
      case (l,t) => {
        val (cI,s) = classify(t)
        val label = labels(cI)
        //GenClassifier.rocW.write(label + "," + d.getMeta("goldLabel") + "," + s + "\n")
        //println(label + " given to " + d.getMeta("goldLabel"))
        tot += 1.0
        if(label == l)
          correct += 1.0
      }
    })

    correct / tot
  }

  def getSentAccuracy(dox : Array[XMLDoc[ParseTree]]) = {

    var correct = 0.0
    var tot = 0.0

    dox.foreach(d => {
      d.text.foreach(t => {
        val (cI,s) = classify(t)
        val label = labels(cI)
        //GenClassifier.rocW.write(label + "," + d.getMeta("goldLabel") + "," + s + "\n")
        //println(label + " given to " + d.getMeta("goldLabel"))
        tot += 1.0
        if(label == d.getMeta("goldLabel"))
          correct += 1.0
      })
    })

    correct / tot

  }

  def getDocAccuracy(dox : Array[XMLDoc[ParseTree]]) = {

    var correct = 0.0

    dox.foreach(d => {
      val (cI,s) = classify(d)
      val label = labels(cI)

      //println(label + " given to " + d.getMeta("goldLabel"))
      if(label == d.getMeta("goldLabel"))
        correct += 1
    })

    correct / dox.length.toDouble
    
  }


}
