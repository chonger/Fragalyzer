import scala.collection.mutable.HashMap

object HELP {

  def pUniform(m : Array[Int]) = {
    val alpha = 1.0
    
    import cc.mallet.util.Maths
    import cc.mallet.types.Dirichlet

    val uni = 1.0 / m.length.toDouble

    def lnorm(alpha : Double) = {
      var norm = (0.0 /: m)((a,b) => a + Dirichlet.logGamma(b+alpha)) 
      norm -= Dirichlet.logGamma((0.0 /: m)(_ + _ + alpha)) 
      norm
    }

    lnorm(10.0) / lnorm(.001)
  }

  //TODO : This depends on hY being uniform 1/4!!!!
  def iginternal(m : Array[Int], tot : Array[Int]) = {

    val tt = (0.0 /: tot)(_ + _)
    val f = (0.0 /: m)(_ + _)
      
    val pU = f/tt

    val hY =  -math.log(.25)
    val hX = ent(f,tt)

    var igain = pU * (0.0 /: m)((a,b) => {
      val p = b / f
      if(p > 0)
        a + p * math.log(p)
      else
        a
    })

    igain += (1-pU) * (0.0 /: (m zip tot))((a,b) => {
      val p = (b._2 - b._1) / (tt - f)
      if(p > 0)
        a + p * math.log(p)
      else
        a
    })

    (hY + igain,hX,hY)
  }

  def su(m : Array[Int], tot : Array[Int]) = {
    val (igain,hX,hY) = iginternal(m,tot)
    2.0 * igain / (hX + hY)
  }

  def ig(m : Array[Int], tot : Array[Int]) = {
    iginternal(m,tot)._1
  }

  def ce(m : Array[Int]) = {
    val tot = (0.0 /: m)(_ + _)
    (0.0 /: m)((a,b) => {
      if(b > 0) {
        val p = b/tot
        a - p*math.log(p)
      } else {
        a
      }
    })
  }

  def chi2(m : Array[Int], p : Array[Double]) = {
    
    val n = (0 /: m)(_ + _)
    val av = p.map(_ * n)

    (0.0 /: 0.until(m.length))((a,b) => {
      a + math.pow(m(b).toDouble - av(b),2)/av(b)
    })
  }

  def altchi2(yes : Array[Int], tots : Array[Int]) = {
    
    val nos = (tots zip yes).map(x => x._1.toDouble - x._2.toDouble)

    val sumYes = (0.0 /: yes)(_ + _)
    val sumNo = (0.0 /: nos)(_ + _)
    val total = (0.0 /: tots)(_ + _)
    
    var x2 = 0.0
    0.until(yes.length).foreach(i => {
      val eY = sumYes * tots(i) / total
      x2 += math.pow(eY - yes(i),2.0) / eY
      val eN = sumNo * tots(i) / total
      x2 += math.pow(eN - nos(i),2.0) / eN
    })
    x2
  }

  def ent(c : Double, t : Double)= {
    val  p = c/t
    if(p == 0)
      0
    else if(p == 1)
      0
    else
      -p*math.log(p)-(1.0-p)*math.log(1.0-p)
  }
  
  def chi2LD(counts : Array[Array[Int]]) = {

    val totalC : Double = (0.0 /: counts)((a,b) => a + (0.0 /: b)(_ + _))

    val nD = counts.length
    val nL = counts(0).length


    val exD = 0.until(nD).map(d => {
      (0.0 /: 0.until(nL))((a,b) => a + counts(d)(b))
    })
    val exL = 0.until(nL).map(l => {
      (0.0 /: 0.until(nD))((a,b) => a + counts(b)(l))
    })

    val expected = Array.tabulate(nD,nL)((d,l) => exD(d) * exL(l) / totalC)

    var x2 = 0.0

    0.until(nD).foreach(d => {
      0.until(nL).foreach(l => {
        val e = expected(d)(l)
        x2 += math.pow(e - counts(d)(l),2.0) / e
      })
    })

    x2
  }  

  def altchi2LD(counts : Array[Array[Int]], tots : Array[Array[Int]], occ : Array[Int], ltot : Array[Int]) = {

    val nD = counts.length
    val nL = counts(0).length

    val fTot = 1000.0
    val factors = tots.map(_.map(_ / fTot))
    val fCounts = Array.tabulate(nD,nL)((x,y) => counts(x)(y) / factors(x)(y))
   
    val fSum = (0.0 /: fCounts)((x,y) => (x /: y)(_ + _))
 
    val exD = 0.until(nD).map(d => {
      (0.0 /: 0.until(nL))((a,b) => a + fCounts(d)(b))
    })
    val exL = 0.until(nL).map(l => {
      (0.0 /: 0.until(nD))((a,b) => a + fCounts(b)(l))
    })

    

    val expected = Array.tabulate(nD,nL)((d,l) => exD(d) * exL(l) / fSum * factors(d)(l))

    var x2 = 0.0

    0.until(nD).foreach(d => {
      0.until(nL).foreach(l => {
        val e = expected(d)(l)
        x2 += math.pow(e - counts(d)(l),2.0) / e
      })
    })

    x2
  }  

  /**
   *
   *  Demo of different metrics
   *
   */ 

  def main(args : Array[String]) = {


    val ii = Array(20,5,5,5)
    val aa = Array(40,20,20,20)
    val jj = Array(400,100,100,100)
    val kk = Array(1700,1800,1700,1800)
    val oo = Array(9,3,2,6)
    
    val tot = Array(2500,2500,2500,2500)
    val p  = Array(.25,.25,.25,.25)    

    println("A = " + ig(ii,tot) + "\t\t" + su(ii,tot) + "\t" + chi2(ii,p)+ "\t" + ce(ii))
    println("B = " + ig(aa,tot) + "\t\t" + su(aa,tot) + "\t" + chi2(aa,p)+ "\t" + ce(aa))
    println("C = " + ig(jj,tot) + "\t\t" + su(jj,tot) + "\t" + chi2(jj,p)+ "\t" + ce(jj))
    println("D = " + ig(kk,tot) + "\t\t" + su(kk,tot) + "\t" + chi2(kk,p)+ "\t" + ce(kk))
    println("E = " + ig(oo,tot) + "\t\t" + su(oo,tot) + "\t" + chi2(oo,p)+ "\t" + ce(oo))

  }

  
}
