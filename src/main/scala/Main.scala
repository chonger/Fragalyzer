import java.io.File
import multitool._
import scalafrontend._
import scala.collection.mutable.{HashMap,HashSet}
import enbuske._

object Main {

  class Config(var sentSeg : Boolean = false,
               var loadptb : Boolean = false, 
               var loadtsg : Boolean = false, 
               var in : File = new File("."), 
               var swunk : Boolean = false,
               var pcfg : Boolean = false)

  def main(args : Array[String]) : Unit = {
    
    val parser = new scopt.OptionParser[Config]("scopt") {
      head("Fragalyzer", "1.0")
      opt[Unit]("sentseg") action { (_, c) =>
        c.sentSeg = true; c } text("enable sentence sementation")
      opt[File]('f',"folder") required() valueName("<folder>") action { (x, c) =>
        c.in = x ; c} text("input folder containing files for each data type")
      opt[Unit]("loadptb") action { (_, c) =>
        c.loadptb = true ; c} text("load from ptb files")
      opt[Unit]("loadtsg") action { (_, c) =>
        c.loadtsg = true ; c} text("load precomputed tsg rules")
      opt[Unit]("swunk") action { (_, c) =>
        c.swunk = true ; c} text("remove (unk) all tokens except stopwords")
      opt[Unit]("pcfg") action { (_, c) =>
        c.pcfg = true ; c} text("just use PCFG features (very fast)")
    }
    val config = parser.parse(args, new Config()).getOrElse({
      // arguments are bad, error message will have been displayed
      System.exit(-2)
      new Config()
    })

    val st = new CFGSymbolTable()
    var ptb = Array[XMLDoc[ParseTree]]()
    if(!config.loadptb) {
      val files = config.in.listFiles().filter(_.getName().indexOf(".txt") > 0)
      ptb = files.map(f => {
        val fin = io.Source.fromFile(f.getAbsolutePath())
        val label = f.getName().slice(0,f.getName().indexOf("."))
        val tz = fin.getLines.flatMap(sent => {
          try {
            val t = Stanford.parse(sent.trim,st)
            print(".")
            List(t)
          } catch {
            case e : Exception => {
              print("!")
              List[ParseTree]()
            }
          }    
        }).toList
        val ptbfile = f.getAbsolutePath().replaceAll("\\.txt",".ptb")
        st.write(ptbfile,tz)
        fin.close()
        new XMLDoc(tz,Array(("goldLabel",label)))
      })
    } else {
      val files = config.in.listFiles().filter(_.getName().indexOf(".ptb") > 0)
      ptb = files.map(f => {
        val label = f.getName().slice(0,f.getName().indexOf("."))
        val tz = st.read(f.getAbsolutePath())
        new XMLDoc(tz,Array(("goldLabel",label)))
      })
    }

    if(config.swunk) {
      println("Unking all non-stopwords")
      ptb = Unker.swunk(ptb,st)
    }
    
    println("Got " + ptb.length + " labels")
    println("LABEL\t\t\t# Sentences")
    ptb.foreach(x => {
      println(x.getMeta("goldLabel") + "\t\t\t" + x.text.length)
    })

    val tsgFilename = config.in.getAbsolutePath() + "tsg_rules.seg"
    val tsgrules : List[ParseTree] = if(config.pcfg) {
      val pcfg = PTSG.mlPCFG(st,ptb.flatMap(_.text).toList)
      val ret = new HashSet[ParseTree]() 
      ret ++= pcfg.rules.flatMap(_.keySet.iterator.toList)
      ret.toList
    } else if(config.loadtsg) {
      st.read(tsgFilename)
    } else {
      val pcfg = PTSG.mlPCFG(st,ptb.flatMap(_.text).toList)
      val typeA = (new HashSet[String]() ++ ptb.map(_.getMeta("goldLabel"))).toArray
      val nT = typeA.length
      val theta = Array.tabulate(nT)(x => Array(100.0))
      val alphas = Array(100.0)
      val gamma = 1000000.0 //bypasses smoothing grammar
      val sampler = new SemiSampler(ptb,st,pcfg,typeA,alphas,gamma,theta,nT)
      val nIters = 100
      
      val nSamples = 5
      val betweenSamples = 10
      
      sampler.doSampling(nIters,null)

      val ret = new HashSet[ParseTree]() 
      val newPTSGs = sampler.getPTSGs()
      ret ++= newPTSGs(0).rules.flatMap(_.keySet.iterator.toList)
            
      st.write(tsgFilename,ret.toList)
      ret.toList
    } 

    println("Got " + tsgrules.length + " features")

    val tsgEx = new TSGExtractor(tsgrules.toList)

    println("Extracting Features")
    val dataA : Array[(String,HashMap[ParseTree,Double])] = ptb.flatMap(x => {
      val l = x.getMeta("goldLabel")
      println("working on " + l)
      x.text.map(t => {
        (l,tsgEx.extractBinary(t))
      })
    })
    val dataL = dataA.toList

    val aa = MalletClass.xval[ParseTree](dataL,5)
    val ex = new MalletClass[ParseTree]().train(dataL).extremeFeatures()

    println("Average Accuracy : " + aa)
    println()
    println("Extreme Features")
    ex.foreach({
      case (lbl,fs) => {
        println()
        println("LABEL : " + lbl)
        fs.foreach(f => {
          println(f._2 + "\t\t" + f._1.fString(st))
        })
      }
    })

  }

}


class TSGExtractor(rules : List[ParseTree]) extends FExtractor[ParseTree,ParseTree] {

  val cod = new Compacter(rules)

  override def extract(d : ParseTree) : List[ParseTree] = {
    d.nonterminals.flatMap(n => {
      try {
        val oles = cod.findOverlays(n)
        oles.map(_._1)
      } catch {
        case _ : Throwable => Nil
      }
    })
  }

}
