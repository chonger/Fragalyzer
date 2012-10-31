import multitool._
import scala.collection.mutable.{HashSet,HashMap,ArrayBuffer}

abstract class FExtractor[A] {

  var st = new CFGSymbolTable()
  
  def ex(s : ParseTree) : List[A] 
  def show(a : A) : String
  def find(f : A, a : Array[ParseTree]) : Unit
  def classifier() = new MalletClass[A](this)
  def classifier(d : Double) = new MalletClass[A](this,Some(d))

  def featsBySent(fname : String) = {
    val dox = XMLDoc.read(fname,st)
    val items = dox.par.flatMap(d => {
      val lbl = d.getMeta("goldLabel")
      d.text.map(s => {
        val hs = new HashSet[A]()
        hs ++= ex(s)
        (lbl,hs)
      })
    }).toList
    items
  }

  def featsBySentDoc(fname : String) = {
    val dox = XMLDoc.read(fname,st)
    val items = dox.map(d => {
      val lbl = d.getMeta("goldLabel")
      val ss = d.text.map(s => {
        val hs = new HashSet[A]()
        hs ++= ex(s)
        hs
      }).toList
      (lbl,ss)
    }).toList
    items
  }

  def featsByDoc(fname : String) = {
    val dox = XMLDoc.read(fname,st)
    val items = dox.par.map(d => {
      val lbl = d.getMeta("goldLabel")
      val hs = new HashSet[A]()
      d.text.map(s => {
        hs ++= ex(s)
      })
      (lbl,hs)
    }).toList
    items
  }

}

class PCFGExtractor extends FExtractor[NonTerminalNode] {
  def ex(s : ParseTree) = s.nonterminals.map({
    case p : ProtoNode => new ProtoNode(p.symbol,p.children.map(x => new UnderspecifiedNode(x.symbol,null)))
    case ptn : PreTerminalNode => ptn
  })
  def show(a : NonTerminalNode) = a.toString(st)
  def find(f : NonTerminalNode, a : Array[ParseTree]) : Unit = {

  }
}

class BNPExtractor extends FExtractor[ProtoNode] {

  //def compress(n : ProtoNode) : ProtoNode = nnpcompress(n,st)
  def compress(n : ProtoNode) : ProtoNode = BNP.nnjjcompress(n,st)
  //def compress(n : ProtoNode) : ProtoNode = {n}

  def ex(s : ParseTree) : List[ProtoNode] = {
    s.getBNPs(st).map(p => {
      compress(new ProtoNode(p.symbol,p.children.map(x => new UnderspecifiedNode(x.symbol,null))))
    })
  }
  def show(a : ProtoNode) = a.toString(st)
  def find(f : ProtoNode, a : Array[ParseTree]) : Unit = {
    a.foreach(x => {
      x.getBNPs(st).foreach(p => {
        if(f == new ProtoNode(p.symbol,p.children.map(x => new UnderspecifiedNode(x.symbol,null)))) {
          val spans = x.getSpans()
          spans.zip(x.nonterminals).foreach(nn => {
            if(nn._2 eq p)
              println(x.sentence(st,nn._1))
          })
          return
        }
      })
    })
  }

  def investigate(filE : String) = {

    val dox = XMLDoc.read(filE,st)
    val fMap = new HashMap[ProtoNode,HashMap[String,(HashSet[String],Int)]]()
    val classTot = new HashMap[String,Int]()

    var ccc = 0
    
    dox.foreach(d => {
      ccc += 1
      println("Scanning " + ccc + " of " + dox.length)
      val lbl = d.getMeta("goldLabel")
      classTot(lbl) = classTot.getOrElse(lbl,0) + d.text.length
      d.text.foreach(t => {
        val bnps = new HashSet[ProtoNode]() ++ t.getBNPs(st)
        bnps.toArray.map(p => {
          val n = compress(new ProtoNode(p.symbol,p.children.map(x => new UnderspecifiedNode(x.symbol,null))))
          val lmap = fMap.getOrElse(n,new HashMap[String,(HashSet[String],Int)]())
          val (strs,c) = lmap.getOrElse(lbl,(new HashSet[String](),0)) 
          lmap(lbl) = (strs + (new ParseTree(p)).sentence(st),c+1)
          fMap += n -> lmap
        })
      })
    })

    val allEx = (0.0 /: classTot.iterator)(_ + _._2)

    println("got " + fMap.size + " features")
    println(allEx + " total examples")

    fMap.iterator.toArray.map({
      case (n,m) => {
        val cFire = (0.0 /: m.iterator)(_ + _._2._2) //total number of firings for this feature
        val cNoFire = allEx - cFire
        val pFire = cFire / allEx

        var onE = 0.0
        var offE = 0.0

        classTot.iterator.foreach({
          case (lbl,count) => {
            val lFire = m.getOrElse(lbl,(new HashSet[String](),0))._2
            val nFire = count - lFire
            if(lFire > 0) {
              val p = lFire.toDouble / cFire
              onE = onE - p * math.log(p)
            }
            if(nFire > 0) {
              val p = nFire.toDouble / cNoFire
              offE = offE - p * math.log(p)
            }
          }
        })

        val ig = pFire * onE + (1.0-pFire) * offE
/**
        println(n.toString(st))
        println(m.iterator.toArray.map({
          case (l,(s,c)) => l + "/" + c
        }).mkString(" "))
        println("PFIRE : " + pFire)
        println("onE : " + onE)
        println("offE : " + offE)
        println("IG : " + ig)

        readLine()
*/
        (n,m,ig)
      }
    }).sortWith(_._3 < _._3).slice(0,50).reverse.foreach({
      case (n,m,e) => {
        println("\n" + n.toString(st) + " (" + e + ")")
        m.iterator.foreach({
          case (l,(s,c)) => {
            println(l + "/" + c)
            s.iterator.slice(0,1).foreach(x => println(x))
          }
        })
      }
    })
    
  }

}


class TSGExtractor(tsgF : String) extends FExtractor[ParseTree] {

  val cod = new Compacter(PTSG.read(tsgF,st).rules.toList.flatMap(_.map(_._1)))

  def ex(s : ParseTree) = {
    s.nonterminals.flatMap(n => {
      try {
        val oles = cod.findOverlays(n)
        oles.map(_._1)
      } catch {
        case _ => Nil
      }
    })
  }

  def show(a : ParseTree) = a.fString(st)
  def find(f : ParseTree, a : Array[ParseTree]) : Unit = {

  }

}

