
import scala.collection.mutable.HashSet
import multitool._

object DiscClassifier {

  def main(args : Array[String]) = {

    val unk_bnp = "/home/chonger/data/ICLE/icle_bnp_unk.xml" //unked with collapsed bnps
    val unk_normal = "/home/chonger/data/ICLE/icle_unk.xml" //unked

//    val gram_bnp = "/home/chonger/data/ICLE/8-20-2012/bnp-tsg_2.txt"
    val gram_reg = "/home/chonger/data/ICLE/reg-grammar.txt"

    val xmlF = "/home/chonger/data/NLI/toefl_bp2.xml"
    //val xmlF = "/home/chonger/data/ICLE/icle_cn.xml"
    //val xmlF = unk_normal
    val xml2 = "/home/chonger/data/ICC/xml/bparsed.xml"
    //val sampF = "/home/chonger/data/ICC/xml/debug-sampled.txt"
    
    //


    val ttt = false

    if(ttt) {
      val st = new CFGSymbolTable()
      val tsgex = new TSGExtractor(gram_reg,st)
      val tsgI = tsgex.featsBySent(xmlF)
      val tsgC = tsgex.classifier(.1)
      val tsgA = tsgC.crossVal(tsgI,.8,10)
      println("TSG  - " + tsgA)
    } else {
      val pcfgex = new IgnoreExtractor()
      val pcfgI = pcfgex.featsByDoc(xmlF)
      val pcfgC = pcfgex.classifier(.1)

      val exs = pcfgI.groupBy(_._1)

      val train1 = exs.toList.flatMap(zz => {
        zz._2.slice(0,600)
      })
      println("totoal = " + train1.length)
      val train2 = pcfgex.trim(train1)
      val test = pcfgex.trim(exs.toList.flatMap(zz => {
        zz._2.drop(600)
      }))
      println("tsto=sal = " + test.length)

      println("With Bad F")
      println(pcfgC.check(train1,test))
      println("Without Bad F")
      println(pcfgC.check(train2,test))

      //val pcfgA = pcfgC.crossVal(pcfgI,.8,10)
      //println("PCFG - " + pcfgA)
      //pcfgC.extremes(pcfgI,xmlF)
    }

    //
 
    //ex.investigate(xmlF)

    //val items = new TSGExtractor(xmlF,sampF).featsBySent(xmlF)

    //val items = ex.featsByDoc(xmlF)
      
    //tsgC.extremes(tsgI,xmlF)

    //pcfgC.extremes(pcfgI,xmlF)
      
    //println("XVAL = " + MalletClass.crossValSD(items))
    //GenClassifier.rocW.close()

  }

}
