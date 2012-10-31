
import scala.collection.mutable.HashSet
import multitool._



/**
 *
  *    BNP data S - PCFG/24.3/24.3     TSG/25.3/25.4
  *    REG data S - PCFG/25.2/25.1     TSG/26.4/26.1  
  *
  *    BNP data D - PCFG/56.6/57.2   TSG/65.3/63.4
  *    REG data D - PCFG/56.5/57.0   TSG/63.6/63.3
  *
  *  PCFG REGULARIZER GRID S (REG/SENT)
  * !!!!0.22645687076750184
    !!!!0.2511155728341203     
    !!!!0.24743470617979885
    !!!!0.23798777825118733
    !!!!0.23753197504288542

  *
  */ 



/**
 *     GSJC
  *
  *
  *    DOC/BNP  - PCFG/90.6 G-30.9
  *    SENT/BNP - PCFG/60.7 G-54.5->55.4?
  *
  *    DOC/REG  - PCFG/90.0 G-32.1
  *    SENT/REG - PCFG/62.2 G-55.7->56.5?
  *
  *     D - 39.3  37.8
  */ 


object DiscClassifier {

  def main(args : Array[String]) = {

    val unk_bnp = "/home/chonger/data/ICLE/icle_bnp_unk.xml" //unked with collapsed bnps
    val unk_normal = "/home/chonger/data/ICLE/icle_unk.xml" //unked

//    val gram_bnp = "/home/chonger/data/ICLE/8-20-2012/bnp-tsg_2.txt"
    val gram_reg = "/home/chonger/data/ICLE/reg-grammar.txt"

    //val xmlF = "/home/chonger/data/ICC/xml/bparsed.xml"
    val xmlF = unk_normal
    val xml2 = "/home/chonger/data/ICC/xml/bparsed.xml"
    //val sampF = "/home/chonger/data/ICC/xml/debug-sampled.txt"
    
    //


    val ttt = false

    if(ttt) {
      val tsgex = new TSGExtractor(gram_reg)
      val tsgI = tsgex.featsBySent(xmlF)
      val tsgC = tsgex.classifier(.1)
      val tsgA = tsgC.crossVal(tsgI,.8,10)
      println("TSG  - " + tsgA)
    } else {
      val pcfgex = new PCFGExtractor()
      val pcfgI2 = pcfgex.featsBySent(xml2)
      val pcfgI = pcfgex.featsBySent(xmlF)
      val pcfgC = pcfgex.classifier(.1)
      println(pcfgC.crosscheck(pcfgI,pcfgI2))
      //val pcfgA = pcfgC.crossVal(pcfgI,.8,10)
      //println("PCFG - " + pcfgA)
    }

    //
    
      
 
    //ex.investigate(xmlF)

    //val items = new TSGExtractor(xmlF,sampF).featsBySent(xmlF)



    //val items = ex.featsByDoc(xmlF)
      
    //tsgC.extremes(tsgI,xmlF)


    //pcfgC.extremes(pcfgI,xmlF)


    //
      
    //println("XVAL = " + MalletClass.crossValSD(items))
    //GenClassifier.rocW.close()

  }

}
