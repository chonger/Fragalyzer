import multitool._
import java.io._
import scala.collection.mutable.HashMap

object MakeTex {

  lazy val st = new CFGSymbolTable()

  def main(args : Array[String]) = {

    val treeFiles = Array("/home/chonger/data/NLI/icle_u.xml",
                          "/home/chonger/data/NLI/icci_u.xml",
                          "/home/chonger/data/NLI/L8_u.xml",
                          "/home/chonger/data/NLI/fce_u.xml")

    val ds = List("ICLE","ICCI","LANG8","FCE")
    
    val tsgFile = "/home/chonger/data/NLI/naacl_smart.txt"
    val fSave = "/home/chonger/data/NLI/naacl_save.txt"


    
    val data = treeFiles.map(f => {
      XMLDoc.read(f,st)
    })

    val grammar = PTSG.read(tsgFile,st).rules.toArray.flatMap(_.map(_._1))
    val analyzer = new L1Analyzer(st,data,grammar,fSave)
    val useFrags = analyzer.fullFilter(15,100,.2,5).toArray

    val out = "/home/chonger/Documents/corpus.tex"

    val bw = new BufferedWriter(new FileWriter(out))

    bw.write("\\documentclass[11pt]{article}\n")
    bw.write("\\usepackage{qtree}\n")
    bw.write("\\usepackage{booktabs}\n")
    bw.write("\\usepackage[margin=0.5in]{geometry}\n")
    bw.write("\\begin{document}\n")
    bw.write("\n\\hrulefill\n\n")
//    bw.write("\\begin{tabular}{rl}\n")

    val langs = analyzer.labels

    var c = 0

    useFrags.foreach(frag => {
 //     c += 1
      val counts = new HashMap[(String,Int),Int]()
      val t = analyzer.frags(frag.id)
      println(t.fString(st))

      val xxx = frag.x2
      val ddd = frag.aldX2

      bw.write("\\vspace{1em}\n")
      bw.write("\\begin{array}[t]{p{.33\\textwidth}p{.33\\textwidth}p{.33\\textwidth}}\n")
               //bw.write("\n\\begin{tabular}{cc}\n")      
      bw.write("\\vspace{-4.8em}" + writeT(t) + "&")

      bw.write("\n\\begin{tabular}{ccccc}\n")
      bw.write("\\toprule\n&" + langs.toArray.mkString("&") + "\\\\\n\\midrule\n")
   
      var i = 0
      (frag.occur zip analyzer.lcountsA).foreach(x => {
        val y = (x._1 zip x._2).map(z => z._1.toDouble / z._2.toDouble).map(_ * 1000).map("%.1f" format _)
        bw.write(ds(i)+"&")
        i += 1
        bw.write(y.toArray.mkString("&")+ "\\\\\n")

      })

      bw.write("\\midrule\n")
      val sums = (frag.loccur zip analyzer.lTotals).map(z => z._1.toDouble / z._2.toDouble).map(_ * 1000).map("%.1f" format _)
      bw.write("ALL&")
      bw.write(sums.toArray.mkString("&") + "\\\\\n\\bottomrule\n")
      bw.write("\\end{tabular}&\\vspace{-4em}\\begin{eqnarray*}\n")
      bw.write("\\chi^2_L: " + "%.1f".format(xxx) + "\\\\\n")
      bw.write("\\chi^2_D: " + "%.1f".format(ddd) + "\\\\\n")
      bw.write("\\end{eqnarray*}\\end{array}\n")
      bw.write("\\vspace{1em}\n")
      bw.write("\n\\hrulefill\n\n")

    })

    bw.write("\\end{document}\n")

    bw.close()

  }

  def writeT(t : ParseTree) = {
    val rrr = "[.. ]"

    def isAt(n : NonTerminalNode) ={
      st.syms(n.symbol).indexOf("@") == 0
    }
    
    def getAt(n : NonTerminalNode) ={
      st.syms(n.symbol).drop(1)
    }

    def recR(n : NonTerminalNode) : String = {
      n match {
        case p : ProtoNode => {
          if(isAt(n))
            " " + p.children.map(x => recR(x)).mkString(" ") + " " 
          else
          "[." + st.syms(p.symbol).replaceAll("\\.",rrr) + " " + p.children.map(x => recR(x)).mkString(" ") + " ]"
        }
        case u : UnderspecifiedNode => {
          if(isAt(n))
            "*"
          else
            st.syms(u.symbol).replaceAll("\\.",rrr)
        }
        case ptn : PreTerminalNode => {
          "[." + st.syms(ptn.symbol).replaceAll("\\.",rrr) + " " + st.terms(ptn.kid.terminal) + " ]"
        }
      }
    }

    if(isAt(t.root)) {
      var s = "\\Tree [." + getAt(t.root) + " "
      s += t.root.asInstanceOf[ProtoNode].children.map(x => recR(x)).mkString(" ") + " * ]"
      s.replaceAll("\\$","\\\\\\$")
    } else {
      ("\\Tree " + recR(t.root)).replaceAll("\\$","\\\\\\$")
    }
  }


}
