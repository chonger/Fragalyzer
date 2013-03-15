import multitool._
import cc.mallet.types.Dirichlet
import scala.collection.mutable.{HashMap,HashSet}

//56.2

object VB {

  def main(args : Array[String]) : Unit = {
    val tsg1 = "/home/chonger/data/ICLE/icle-tsg.txt"
    val tsg2 = "/home/chonger/data/ICC/xml/icci-tsg.txt"

    val st = new CFGSymbolTable()
    val dox1 = XMLDoc.read("/home/chonger/data/ICLE/icle_u.xml",st)
    val dox2 = XMLDoc.read("/home/chonger/data/ICC/xml/icci_u.xml",st)

    //val base = PTSG.read(tsg1,st)
    //val tsgs = base.rules.flatMap(_.map(_._1)).toList

    val cfgs = TreeTools.cfgSet((dox1.toList ::: dox2.toList).flatMap(_.text).toList).toList
    
    val grammar = new HashSet[ParseTree]()
    grammar ++= PTSG.read(tsg1,st).rules.toList.flatMap(_.map(_._1))
    grammar ++= PTSG.read(tsg2,st).rules.toList.flatMap(_.map(_._1))
    grammar ++= cfgs

    val m1 = dox1.groupBy(_.getMeta("goldLabel"))
    val m2 = dox2.groupBy(_.getMeta("goldLabel"))
    
    val lll = m1.map(_._1).toArray    

    //learn(lll,st,lll.map(x => m1(x).flatMap(_.text).toList),lll.map(x => m2(x).flatMap(_.text).toList),grammar.toList)
    learn(lll,st,lll.map(x => m1(x).flatMap(_.text).toList),lll.map(x => m2(x).flatMap(_.text).toList),cfgs)

    //println(GenClassifier.pcfgXVAL(dox,st,cfgs))
    //println(GenClassifier.tsgXVAL(dox,st,base))
    //println(GenClassifier.genXVAL(dox,st,x => VB.learn(st,x,tsgs)))

  }

  def learn(labels : Array[String],st : CFGSymbolTable, trees : Array[List[ParseTree]], grammar : List[ParseTree]) : Array[PTSG] = {
    learn(labels,st,trees,Array[List[ParseTree]](),grammar)
  }

  def learn(labels : Array[String],
            st : CFGSymbolTable, 
            lTrees : Array[List[ParseTree]], 
            uTrees : Array[List[ParseTree]],
            grammar : List[ParseTree]) : Array[PTSG] = {

    val trees = (lTrees zip uTrees).map({
      case (l,u) => {
        (l.toList ::: u.toList).toArray
      }
    })
    val lLengths = lTrees.map(_.length)
    
    val nLabeled = lTrees.length

    var theta = trees.map(_.map(a => {
      Array(.5,.5)
    }).toArray)

    val cod = new Compacter(grammar)

    val beta = Array.tabulate(st.syms.size)(x => new HashMap[ParseTree,Array[Double]]())

    val nTypes = trees.length
    val shareInd = nTypes

    grammar.foreach(g => {
      beta(g.root.symbol) += g -> Array.tabulate(nTypes+1)(x => .1)
    })

    val thetaPrior = 10.0
    val betaPrior = 1.0//.001
    val BOOST = 100

    type OLEMap = HashMap[RefWrapper,List[(ParseTree,List[NonTerminalNode])]]

    def getOverlays(tree : ParseTree) = {
      val overlays = new OLEMap()
      overlays ++= tree.nonterminals.map(n => {
        (new RefWrapper(n),cod.findOverlays(n))
      })
      overlays
    }

    def getProb(t : ParseTree, typeI : Int, treeI : Int) : Double = {
      val genP = theta(typeI)(treeI)(0) * beta(t.root.symbol)(t)(shareInd)
      val specP = theta(typeI)(treeI)(1) * beta(t.root.symbol)(t)(typeI)
      (genP + specP) * math.pow(BOOST,t.preterminals.length)
    }

    def getProb2(t : ParseTree, typeI : Int, treeI : Int, ty : Int) : Double = {
      val genP = theta(typeI)(treeI)(0) * beta(t.root.symbol)(t)(shareInd)
      val specP = theta(typeI)(treeI)(1) * beta(t.root.symbol)(t)(ty)
      (genP + specP) * math.pow(BOOST,t.preterminals.length)
    }
    import java.io._
    var bw : BufferedWriter = null

    def emIter() = {

      print("EM ")

      val thetaX = theta.map(_.map(x => Array(thetaPrior,thetaPrior)))
      val betaX = Array.tabulate(st.syms.size)(x => new HashMap[ParseTree,Array[Double]]())
      grammar.foreach(g => {
        betaX(g.root.symbol) += g -> Array.tabulate(nTypes+1)(x => betaPrior)
      })

      var acc = 0.0
      var tot = 0.0

      0.until(trees.length).foreach(typeI => {
        val tz = trees(typeI)

        0.until(tz.length).foreach(treeI => {
          
          val tree= tz(treeI)

          val overlays = getOverlays(tree)

          if(treeI < lLengths(typeI)) {

            //INSIDES

            val insideMap = new HashMap[RefWrapper,Double]()
            
            def inside(n : NonTerminalNode) : Double = {
              val rw = new RefWrapper(n)
              insideMap.getOrElseUpdate(rw,{
                (0.0 /: overlays(rw))({
                  case (a,(t,l)) => {
                    val p = getProb(t,typeI,treeI)
                    a + (p /: l)((x,y) => x * inside(y))
                  }
                })
              })
            }

            inside(tree.root)

            //OUTSIDES

            val outsideMap = new HashMap[RefWrapper,Double]()

            outsideMap += new RefWrapper(tree.root) -> 1.0
            
            tree.nonterminals.foreach(n => {
              val rw = new RefWrapper(n)
              val out = outsideMap(rw)
              overlays(rw).foreach({
                case (t,l) => {
                  val lRW = l.map(ll => new RefWrapper(ll)).toArray //refwrappers
                  val insides = lRW.map(ll => insideMap(ll)) // inside probs for ntLeafs
                  val outP = out * getProb(t,typeI,treeI)
                  0.until(insides.length).foreach(lI => { //add in the outside for each ntLeaf
                    val myRW = lRW(lI)
                    val newO = (outP /: 0.until(insides.length))((a,i) => {
                      if(i == lI)
                        a
                      else
                        a * insides(i)
                    })
                    outsideMap(myRW) = outsideMap.getOrElse(myRW,0.0) + newO
                  })
                }
              })
            })
            
            //EXPECTATIONS

            val norm = insideMap(new RefWrapper(tree.root))

            tree.nonterminals.flatMap(n => {
              
              val rw = new RefWrapper(n)
              val out = outsideMap(rw)
              
              overlays(rw).map({
                case (t,l) => {
                  val lRW = l.map(ll => new RefWrapper(ll)).toArray
                  val insides = lRW.map(ll => insideMap(ll))
                  val outP = out * math.pow(BOOST,t.preterminals.length) 
                  val genP = theta(typeI)(treeI)(0) * beta(t.root.symbol)(t)(shareInd)
                  val specP = theta(typeI)(treeI)(1) * beta(t.root.symbol)(t)(typeI)
                  val genOut = ((outP * genP) /: insides)(_ * _)
                  val specOut = ((outP * specP) /: insides)(_ * _)
                  val gEx = genOut / norm
                  val sEx = specOut / norm
                  thetaX(typeI)(treeI)(0) += gEx
                  thetaX(typeI)(treeI)(1) += sEx
                  val xProbs = betaX(t.root.symbol)(t)
                  xProbs(shareInd) += gEx
                  xProbs(typeI) += sEx
                }
              })
            })
          } else { //unlabeled

            //INSIDES

            val iMaps = 0.until(nTypes).map(x => {
              val insideMap = new HashMap[RefWrapper,Double]()
              
              def inside(n : NonTerminalNode) : Double = {
                val rw = new RefWrapper(n)
                insideMap.getOrElseUpdate(rw,{
                  (0.0 /: overlays(rw))({
                    case (a,(t,l)) => {
                      val p = getProb2(t,typeI,treeI,x)
                      a + (p /: l)((x,y) => x * inside(y))
                    }
                  })
                })
              }

              inside(tree.root)
              
              insideMap
            })

            val totalP = iMaps.map(x => x(new RefWrapper(tree.root)))

            val sumP = (0.0 /: totalP)(_ + _)
            
            val propP = totalP.map(_/sumP)

            var best = (0.0,-1)
              
            0.until(nTypes).foreach(x => {
              if(propP(x) > best._1)
                best = (propP(x),x)
            })

            //println(propP.mkString(" "))

            if(best._2 == typeI)
              acc += 1.0
            tot += 1.0

          val th = theta(typeI)(treeI)
          bw.write(labels(best._2) + "," + labels(typeI) + "," + 
                     0.until(labels.length).map(x => labels(x) + "#" + propP(x)).mkString("|") + 
                     "," + th(0) + "," + th(1) + "\n")

            //OUTSIDES

            0.until(nTypes).foreach(x => {
              val propMass = propP(x)
              val insideMap = iMaps(x)
              val outsideMap = new HashMap[RefWrapper,Double]()

              outsideMap += new RefWrapper(tree.root) -> 1.0
              
              tree.nonterminals.foreach(n => {
                val rw = new RefWrapper(n)
                val out = outsideMap(rw)
                overlays(rw).foreach({
                  case (t,l) => {
                    val lRW = l.map(ll => new RefWrapper(ll)).toArray //refwrappers
                    val insides = lRW.map(ll => insideMap(ll)) // inside probs for ntLeafs
                    val outP = out * getProb2(t,typeI,treeI,x)
                    0.until(insides.length).foreach(lI => { //add in the outside for each ntLeaf
                      val myRW = lRW(lI)
                      val newO = (outP /: 0.until(insides.length))((a,i) => {
                        if(i == lI)
                          a
                        else
                          a * insides(i)
                      })
                      outsideMap(myRW) = outsideMap.getOrElse(myRW,0.0) + newO
                    })
                  }
                })
              })
              
              
              //EXPECTATIONS

              val norm = insideMap(new RefWrapper(tree.root))

              tree.nonterminals.flatMap(n => {
                
                val rw = new RefWrapper(n)
                val out = outsideMap(rw)
                
                overlays(rw).map({
                  case (t,l) => {
                    val lRW = l.map(ll => new RefWrapper(ll)).toArray
                    val insides = lRW.map(ll => insideMap(ll))
                    val outP = out * math.pow(BOOST,t.preterminals.length) 
                    val genP = theta(typeI)(treeI)(0) * beta(t.root.symbol)(t)(shareInd)
                    val specP = theta(typeI)(treeI)(1) * beta(t.root.symbol)(t)(x)
                    val genOut = ((outP * genP) /: insides)(_ * _)
                    val specOut = ((outP * specP) /: insides)(_ * _)
                    val gEx = genOut / norm * propMass 
                    val sEx = specOut / norm * propMass
                    thetaX(typeI)(treeI)(0) += gEx
                    thetaX(typeI)(treeI)(1) += sEx
                    val xProbs = betaX(t.root.symbol)(t)
                    xProbs(shareInd) += gEx
                    xProbs(x) += sEx
                  }
                })
              })
            })
             
          }   
        })

      })

      0.until(trees.length).foreach(tyI => {
        0.until(trees(tyI).length).foreach(trI => {
          val tX = thetaX(tyI)(trI)
          val sum = (0.0 /: tX)(_ + _)
          theta(tyI)(trI)(0) = math.exp(Dirichlet.digamma(tX(0)) - Dirichlet.digamma(sum))
          theta(tyI)(trI)(1) = math.exp(Dirichlet.digamma(tX(1)) - Dirichlet.digamma(sum))
        })
      })

      betaX.foreach(b => {
        0.until(nTypes+1).foreach(gI => {
          val sum = (0.0 /: b.iterator)(_ + _._2(gI))
          b.foreach({
            case (t,p) => {
              beta(t.root.symbol)(t)(gI) = math.exp(Dirichlet.digamma(p(gI)) - Dirichlet.digamma(sum))
              //beta(t.root.symbol)(t)(gI) = p(gI)/sum
            }              
          })
        })
      })

      if(tot > 0.0)
        println("ACC : " + (acc/tot))

      val props = 0.until(trees.length).map(x => {
        val (gen,spec) = ((0.0,0.0) /: theta(x))((a,b) => {
          (a._1 + b(0),a._2 + b(1))
        })
        val nTz = theta(x).length.toDouble
        (gen/nTz,spec/nTz)
      })

      println("average GEN/SPEC proportions")
      println(props.mkString("\n"))

    }


    0.until(30).foreach(iter => {
      bw = new BufferedWriter(new FileWriter("/home/chonger/dump/VB-ROC-" + iter + ".txt"))
      emIter()
      bw.close()
    })


    val props = 0.until(trees.length).map(x => {
      val (gen,spec) = ((0.0,0.0) /: theta(x))((a,b) => {
        (a._1 + b(0),a._2 + b(1))
      })
      val nTz = theta(x).length.toDouble
      (gen/nTz,spec/nTz)
    })

    println("average GEN/SPEC proportions")
    println(props.mkString("\n"))

    val ptsgs = 0.until(trees.length).map(tyI => {
      val (gen,spec) = props(tyI)
      
      val rules : Array[HashMap[ParseTree,Double]] = beta.map(_.map({
        case (r,ps) => {
          val gP = gen * ps(shareInd)
          val sP = spec * ps(tyI)
          (r,gP + sP)
        }
      }))

      new PTSG(st,rules)
    }).toArray
    
    ptsgs
  }
}
