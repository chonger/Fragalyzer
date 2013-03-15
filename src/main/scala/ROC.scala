import scala.collection.mutable.{HashMap,HashSet}
import java.io._

object ROC {

  def main(args : Array[String]) = {
        /**
    val fs = List(("/home/chonger/dump/roc-disc-raw.txt","/home/chonger/dump/Groc-disc.txt"),
    ("/home/chonger/dump/VB-PCFG1-RAW.txt","/home/chonger/dump/G-PCFG1.txt"),
    ("/home/chonger/dump/roc-gen-raw.txt","/home/chonger/dump/Groc-gen.txt"),
    ("/home/chonger/dump/VB-PCFG-RAW.txt","/home/chonger/dump/G-PCFG.txt"))
*/
    val fs = List(
      //("/home/chonger/dump/roc-disc-tsg-raw.txt","/home/chonger/dump/GTroc-disc.txt"),
      ("/home/chonger/dump/VB1-ROC-2.txt","/home/chonger/dump/1-TSG.txt")
      //("/home/chonger/dump/VBD-ROC-2.txt","/home/chonger/dump/D-TSG.txt")
      //("/home/chonger/dump/roc-gen-tsg-raw.txt","/home/chonger/dump/GTroc-gen.txt")
    )
   

      fs.foreach({
        case (inF,out) => ROC.precise(inF,out)
      })
    //val inF = "/home/chonger/dump/VB-PCFG1-RAW.txt"
    //val out = "/home/chonger/dump/G-PCFG-IND.txt"



    //val out2 = "/home/chonger/dump/VB2.txt"

    
    //ROC(inF,out2,true)

  }

  def apply(in : String, out : String) : Unit = {
    apply(in,out,false)
  }

  def getDecisions(filE : String) = {
    io.Source.fromFile(filE).getLines.toList.map(l => {
      val parts = l.trim.split(",")
      val choice = parts(0)
      val gold = parts(1)
      val score = parts(2)
      val (g,s) = if(parts.length > 3) {
        val s = parts(4).toDouble
        (parts(3).toDouble,s)
      } else {
        (-1.0,-1.0)
      }
       (choice,gold,score,g,s)
    })
  }

  def precise(in : String, out :String) : Unit = {

    val decisions = getDecisions(in)

    val labels = decisions.groupBy(_._2).map(_._1).toArray

    val bw = new BufferedWriter(new FileWriter(out))
    0.until(100).foreach(i => {
      var cut = i.toDouble*.01

      val myDec = decisions.filter({
        case (ch,gd,sc,g,s) => {
          var maxD = (0.0 /: sc.split("\\|"))((a,b) => {
            math.max(a,b.split("#")(1).toDouble)
          })
          maxD >= cut
        }
      })
      
      var a = 0.0
      var f = 0.0

      labels.foreach(l => {
        
        var acc = 0.0
        var tot = 0.0
        myDec.filter(_._2 == l).foreach(x => {
          //println(x)
          if(x._1 == x._2)
            acc += 1
          tot += 1
        })
        val myA = acc/tot
        //println(myA)
        if(tot > 0) {
          a += acc/tot
          f += 1
        }
      })
                
      a /= f

      bw.write(a + " " + myDec.length + "\n")
    })

    bw.close()
  }

  def apply(in : String, out : String, gs : Boolean) : Unit = {   

    var avgS = 0.0
    var sT = 0.0


    val decisions = getDecisions(in)

    val classes = new HashSet[String]()
    decisions.foreach(d => classes += d._2)

    println(decisions.groupBy(_._2).map(x => x._1 + " - " + x._2.length).toArray.mkString("\n"))

    println(classes.iterator.toArray.mkString(" "))

    val nC = classes.size.toDouble

    0.until(100).foreach(i => {
      val cut = i.toDouble*.01
      var tpr = 0.0
      var fpr = 0.0
      classes.iterator.foreach(c => {
        //println(c)
        var tp = 0.0
        var fp = 0.0
        var tn = 0.0
        var fn = 0.0
        decisions.foreach({
          case (ch,gd,sc,g,s) => {
            var myS = 0.0
            val scores = sc.split("\\|").foreach(x => {
              val p = x.split("#")
              if(p(0) == c)
                myS = p(1).toDouble
            })
        
            if(myS > cut) { //positive
              if(c == gd)
                tp += 1
              else
                fp += 1
            } else { //negative
              if(gd == c)
                fn += 1
              else
                tn += 1
            }
          }
        })
//        println("!")
  //      println(tp)
    //    println(fp)
      //  println(tn)
        //println(fn)
        tpr += tp/(tp+fn)
        fpr += fp/(fp+tn) 
      })
      //bw.write(fpr/nC + " " + tpr/nC + "\n")
    })
    
//    bw.close()

  }

}
