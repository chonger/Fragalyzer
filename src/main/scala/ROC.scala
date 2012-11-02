import scala.collection.mutable.{HashMap,HashSet}
import java.io._

object ROC {

  def apply(in : String, out : String) = {

    val classes = new HashSet[String]()

    val decisions = io.Source.fromFile(in).getLines.toList.map(l => {
      val parts = l.trim.split(",")
      val choice = parts(0)
      val gold = parts(1)
      val score = parts(2)
      println(l)
      classes += gold
      (choice,gold,score)
    })

    println(classes.iterator.toArray.mkString(" "))

    val bw = new BufferedWriter(new FileWriter(out))

    val nC = classes.size.toDouble

    0.until(100).foreach(i => {
      val cut = i.toDouble*.01
      var tpr = 0.0
      var fpr = 0.0
      classes.iterator.foreach(c => {
        println(c)
        var tp = 0.0
        var fp = 0.0
        var tn = 0.0
        var fn = 0.0
        decisions.foreach({
          case (ch,gd,sc) => {

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
        println("!")
        println(tp)
        println(fp)
        println(tn)
        println(fn)
        tpr += tp/(tp+fn)
        fpr += fp/(fp+tn) 
      })
      bw.write(fpr/nC + " " + tpr/nC + "\n")
    })
    
    bw.close()

  }

}
