import multitool._
import java.io._
import scala.collection.mutable.HashMap

object MakeTex {

    val st = new CFGSymbolTable()
  
  def main(args : Array[String]) = {
    
    val file = "/home/chonger/dump/X.txt"
    val out = "/home/chonger/Documents/corpus.tex"

    val br = new BufferedReader(new FileReader(file))
    val bw = new BufferedWriter(new FileWriter(out))

    bw.write("\\documentclass[11pt]{article}\n")
    bw.write("\\usepackage{qtree}\n")
    bw.write("\\usepackage{booktabs}\n")
    bw.write("\\usepackage[margin=0.5in]{geometry}\n")
    bw.write("\\begin{document}\n")
    bw.write("\n\\hrulefill\n\n")
//    bw.write("\\begin{tabular}{rl}\n")


    br.readLine()
    var line = br.readLine()

    val langs = List("GE","SP","JP","CN")
    val ds = List("ICLE","ICCI","LANG8","FCE")

    var c = 0

    while(line != null && c < 20) {
 //     c += 1
      val counts = new HashMap[(String,Int),Int]()
      val t = st.growTree(line)
      println(t.fString(st))
      br.readLine()
      br.readLine().trim.split(" ").drop(1).foreach(x => {
        val parts = x.split("-")
        val l = parts(0)
        val d = parts(1).toInt
        val c = parts(2).toInt
        val k = (l,d)
        counts(k) = c
      })
      br.readLine()
      br.readLine()
      val xxx = br.readLine().split(" ")(1).toDouble
      val ddd = br.readLine().split(" ")(1).toDouble
      br.readLine()
      line = br.readLine() 
      bw.write("\\vspace{1em}\n")
      bw.write("\\begin{array}[t]{p{.33\\textwidth}p{.33\\textwidth}p{.33\\textwidth}}\n")
               //bw.write("\n\\begin{tabular}{cc}\n")      
      bw.write("\\vspace{-4.8em}" + writeT(t) + "&")

      bw.write("\n\\begin{tabular}{ccccc}\n")
      bw.write("\\toprule\n&" + langs.toArray.mkString("&") + "\\\\\n\\midrule\n")
      val sums = new HashMap[String,Int]()
      0.until(4).foreach(i => {

        bw.write(ds(i)+"&")
        val s = langs.map(l => {
          val c = counts((l,i))
          sums(l) = sums.getOrElse(l,0) + c
          c
        }).toArray.mkString("&")
        bw.write(s + "\\\\\n")
      })
      bw.write("\\midrule\n")
      bw.write("SUM&")
      bw.write(langs.map(l => {
        sums(l)
      }).toArray.mkString("&") + "\\\\\n\\bottomrule\n")
      bw.write("\\end{tabular}&\\vspace{-4em}\\begin{eqnarray*}\n")
      bw.write("\\chi^2_L: " + "%.1f".format(xxx) + "\\\\\n")
      bw.write("\\chi^2_D: " + "%.1f".format(ddd) + "\\\\\n")
      bw.write("\\end{eqnarray*}\\end{array}\n")
      bw.write("\\vspace{1em}\n")
      bw.write("\n\\hrulefill\n\n")

    }

    bw.write("\\end{document}\n")
    br.close()
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
