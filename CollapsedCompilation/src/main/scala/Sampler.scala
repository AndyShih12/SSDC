/**
 */
import scala.util.Random
import sdd.{Sdd, WmcManager, Vtree}
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Set}
import util.control.Breaks._
import java.io.{File, FileWriter, BufferedWriter}
import System.nanoTime

object Sampler {
  var propagate_time = 0L
  val random = new Random()
  // Dists contains distance on graph between 2 variables (based on index)
  var dists: Array[Array[Int]] = null
  var varIndMap: Map[VarNode, Int] = Map()

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    propagate_time += (t1 - t0)
    result
  }

  // Computes all marginals, using size ordering for factors, MI based sampling, and SDD proposal distribution
  def allMarginalsOnlineSampling(fg: FactorGraph, time:Long, thresh: Int,
                                 getPropSample: (VarNode, FactorGraph, WmcManager, List[VarNode], List[Int]) ⇒ Tuple2[Int, Double],
                                 base: Sdd = null): List[Double] = {
    // Create appropriate list of functions
    var funcs: List[Map[VarNode, List[Boolean]]] = List()
    for(i ← 0 to fg.vars.length - 1) {
      // Inner loop for creating appropriate list of functions
      for(j ← 0 to fg.vars(i).n-1) {
        var func: Map[VarNode, List[Boolean]] = Map()
        // Need to add each variable to our function
        for(k ← 0 to fg.vars.length-1) {
          if(i == k) {
            func += (fg.vars(k) → (for(h ← List.range(0, fg.vars(k).n)) yield h == j))
          } else {
            func += (fg.vars(k) → (for(h ← List.range(0, fg.vars(k).n)) yield true))
          }
        }
        funcs ::= func
      }
    }

    val preSdd = if(base == null) {
      Compiler.constructValSDD(Compiler.getManager(0), true)
    } else {
      base
    }

    // Going to take one set of samples and use it for all marginals
    val margs = doOnlineImportanceSampling(fg, funcs, time, thresh, facOrderingRandomBreadthFirst, nextVarMaximizeFrontierDistance(List()), getPropSample, preComp=(Set(), List(), preSdd), doPreComp=false)
    margs.toList
  }
  // Online sampling, a few things here are made generic:
  // 1: Which order we choose to look at the factors in (currently decided ahead of time, later could change)
  // 2: Which variable should be sampled once we reach our threshold
  // 3: The proposal distribution for a variable being sampled out
  def doOnlineImportanceSampling(fg: FactorGraph, functions: List[Map[VarNode, List[Boolean]]], maxSamples: Long, thresh: Int,
                                 getOrder: (FactorGraph) ⇒ List[FactorNode],
                                 getNextVar: (Sdd, FactorGraph, List[VarNode], Set[FactorNode]) ⇒ VarNode,
                                 getPropSample: (VarNode, FactorGraph, WmcManager, List[VarNode], List[Int]) ⇒ Tuple2[Int, Double],
                                 preComp: (Set[VarNode], List[FactorNode], Sdd) = null,
                                 base: Sdd = null,
                                 checkProp: Boolean = false,
                                 res: String = "",
                                 strictSample: Boolean = false,
                                 checkFirstProp: List[Double] = List(),
                                 compZ: Boolean = false,
                                 mem_oracle: WmcManager = null,
                                 doPreComp: Boolean = true
                                 ): Array[Double] = {

    // Construct mapping from vars → ind
    for(i ← fg.vars.indices) {
      varIndMap += (fg.vars(i) → i)
    }
    // Very first thing we should do is precompile all factors
    val facSDD: Map[FactorNode, Sdd] = fg.getFacSDDs
    println("Done compiling individual factors")
    // Going to precompile SDD up until the first time we need to sample, start each iteration from there, only if it's not given
    // Store what we're compiling, ordering of factors for later, varnode → factornode list mapping
    var preCompiled: Set[VarNode] = null
    var preCompiledFacs: List[FactorNode] = null
    var preSdd: Sdd = null
    val facOrder: List[FactorNode] = getOrder(fg)
    val varmap: Map[VarNode, ListBuffer[FactorNode]] = fg.getVarMap
    if(preComp == null && doPreComp) {
      preCompiled = Set[VarNode]()
      preCompiledFacs = List[FactorNode]()
      // Sdd accumulator - begin from our base distribution, unless it's null in whiche case start from true
      if (base == null) {
        preSdd = Compiler.constructValSDD(Compiler.getManager(0), true)
      } else {
        preSdd = base
        base.ref
      }
      breakable {
        for(f ← facOrder) {
          // Get compiled factor and add it (conditioning on evidence)
          facSDD(f).ref
          preSdd = Compiler.conjoinSDD(preSdd, condEvid(facSDD(f), f, fg))
          preCompiledFacs ::= f
          // Add in variables as necessary - ignore if already compiled or in evidence
          for (v ← f.varNodes) {
            if (!preCompiled.contains(v) && !fg.evidence.contains(v)) {
              preSdd = Compiler.conjoinSDD(preSdd, Compiler.compile(v.getCNF))
              preCompiled = preCompiled + v
            }
          }
          // Make sure we're not going over the size limit
          if(preSdd.getSize > thresh) {
            break
          }
        }
        Compiler.garbageCollect
      }
      println("Done precompiling sdd")

    } else if(doPreComp) {
      preCompiled = preComp._1
      preCompiledFacs = preComp._2
      preSdd = preComp._3
    } else {
      preCompiled = Set()
      preCompiledFacs = List()
      preSdd = Compiler.constructValSDD(Compiler.getManager(0), true)
    }

    // Need to actually store the values so logsumexp is as accurate as possible
    var nums: Array[ArrayBuffer[Double]] = Array.fill(functions.length){ArrayBuffer[Double]()}
    var dens: ArrayBuffer[Double] = ArrayBuffer[Double]()

    // Store the unweighted marginals computed for debugging purposes
    var margs: ArrayBuffer[Double] = ArrayBuffer[Double]()

    // To collect stats on # vars sampled and which ones
    var finalNumSamp: ArrayBuffer[Int] = ArrayBuffer[Int]()
    var sampledCount: scala.collection.mutable.Map[VarNode, Int] = scala.collection.mutable.Map() ++ (for (v ← fg.vars) yield (v → 0))

    // Collect stats on marginals
    var trueMarg: ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer[ArrayBuffer[Double]]()
    var propMarg: ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer[ArrayBuffer[Double]]()

    // Tracks which variables were sampled in which iteration, for correcting importance sampler
    var sampledVars: ArrayBuffer[List[VarNode]] = ArrayBuffer[List[VarNode]]()

    val fullSDD: Option[Sdd] = if(checkProp) {
      // Fully compiled SDD to compare to
      println("Compiling full SDD for comparison purposes")
      facSDD.values.map(x ⇒ x.ref())
      Some(Compiler.compile(fg))
    } else {
      None
    }
    println("Done")
    
    while (dens.length < maxSamples) {
    // for(i ← 0 to maxTime.toInt) {
      // Collect stats on marginals
      println("samples: ", dens.length, maxSamples, " time : ", propagate_time/1000000)
      var trueMargIter: ArrayBuffer[Double] = ArrayBuffer[Double]()
      var propMargIter: ArrayBuffer[Double] = ArrayBuffer[Double]()
      // Need to keep track of what we've compiled for sampling and also for compiling new factors
      var compiled: Set[VarNode] = preCompiled.clone()
      var varsAdded: Set[VarNode] = preCompiled.clone()
      var sampled: List[VarNode] = List[VarNode]()
      var samples: List[Int] = List[Int]()
      // Need to keep track of assignments we've made via sampling
      var assignments: ArrayBuffer[Long] = ArrayBuffer[Long]()
      // Sdd accumulator, starts from last thing we compile
      var sdd: Sdd = preSdd
      preSdd.ref()
      // Full SDD "copy"
      var comparsdd: Option[Sdd] = fullSDD
      fullSDD match {
        case Some(sdd) ⇒ sdd.ref()
        case _ ⇒ None
      }

      //println("size of presdd", preSdd.getSize)
      // Proposal distribution probability accumulator
      var q: Double = 0.0
      // Keep track of factors already compiled
      var compiledFacs: List[FactorNode] = preCompiledFacs
      // Go through factors
      for (f ← getOrder(fg).slice(preCompiledFacs.length, facOrder.length)) {
        
        // Generate the list of things we can compile from: variables where all factors are already compiled
        val setcompiled = Set(compiledFacs:_*)
        // var canBeSampled = compiled.filter(v ⇒ varmap(v).forall(f ⇒ setcompiled.contains(f)))
        // println(canBeSampled.map(x ⇒ fg.vars.indexOf(x)))
        // This is where we need to sample stuff, so long as our sdd is too big
        while (sdd.getSize > thresh && compiled.size > 1) {
          // Create our weighted model count manager and set weights
          var wmcman: WmcManager = new WmcManager(sdd, true)
          wmcman = Compiler.setWeights(wmcman, fg, true)
          time { wmcman.propagate() }

          // Choose the variables we're allowed to use based on strictness
          val canBeSampled = if(strictSample && compiled.filter(v ⇒ varmap(v).forall(f ⇒ setcompiled.contains(f))).size > 1) {
            compiled.filter(v ⇒ varmap(v).forall(f ⇒ setcompiled.contains(f))).toList
          } else {
            compiled.toList
          }
          // Sampling
          val toSample: VarNode = getNextVar(sdd, fg, canBeSampled, setcompiled)
          
          sampledCount(toSample) += 1
          // TODO: If we get nan on our sample, should just skip to the next iteration
          val sample: Tuple2[Int, Double] = getPropSample(toSample, fg, wmcman, sampled, samples)


          // Compare to real marginal
          var cowmcman: Option[WmcManager] = comparsdd match {
            case Some(sdd) ⇒
              val temp = Compiler.setWeights(new WmcManager(sdd, true), fg, true)
              time { temp.propagate() }
              Some(temp)
            case _ ⇒ None
          }
          if(checkProp) {
            propMargIter.append(sample._2)
            cowmcman match {
              case Some(wmc) ⇒ trueMargIter.append(Math.exp(wmc.getProbability(toSample.sddIDs(sample._1))))
              case _ ⇒ throw new UninitializedError()
            }
          }

          // Check if we're on the first variable
          if(checkProp && samples.size == 0 || checkFirstProp.length > 0) {
            // First print some summary statistics about the amount of stuff we have to sample from
            println(s"There are ${compiled.size} variables to choose from")
            println(s"There are ${compiled.filter(v ⇒ varmap(v).forall(f ⇒ setcompiled.contains(f))).size} variables which are fully compiled in")
            for(v ← varsAdded) {
              val ind = fg.vars.indexOf(v)
              println(s"Variable $ind: ")
              if(v == toSample) {
                println("This is the variable our heuristic chose")
              }
              println(f"Marginal proposed by currently compiled SDD: ${Math.exp(wmcman.getProbability(v.sddIDs(0)))}%1.5f")
              cowmcman match {
                case Some(cwmcman) ⇒
                  println(f"Marginal according to fully compiled SDD: ${Math.exp(cwmcman.getProbability(v.sddIDs(0)))}%1.5f")
                case _ ⇒
                  if(checkFirstProp.length > 0) {
                    println(f"Marginal according to true marginals given: ${checkFirstProp(ind)}%1.5f")
                  }
              }
              if(varmap(v).forall(f ⇒ setcompiled.contains(f))) {
                println("All factors have been compiled for this variable")
              } else {
                println(s"This variable has ${varmap(v).map(f ⇒ if(setcompiled.contains(f)) 0 else 1).sum} factors which are not yet compiled")
              }
              println(s"This variable is distance ${getDistance(v, fg.vars(0), fg)} from variable 0.")
            }
            if(checkFirstProp.length > 0) {
              println("Exiting")
              return Array[Double]()
            }
          }
          // Update q
          q += Math.log(sample._2)
          // Apply sampled value to sdd, update tracked info
          // Create a log of all the new assignments we're making at an SDD level
          var newAssignments: ArrayBuffer[Long] = new ArrayBuffer[Long]()
          for(i ← 0 to toSample.n - 1) {
            if(i == sample._1) {
              newAssignments.append(toSample.sddIDs(i))
            } else {
              newAssignments.append(-toSample.sddIDs(i))
            }
          }
          // Apply it to the sdd
          sdd = Compiler.conditionSdd(sdd, newAssignments.toArray)
          comparsdd = comparsdd match {
            case Some(sdd) ⇒ Some(Compiler.conditionSdd(sdd, newAssignments.toArray))
            case _ ⇒ None
          }
          // track it
          assignments ++= newAssignments
          sampled ::= toSample
          samples ::= sample._1
          compiled -= toSample
          // canBeSampled -= toSample
          wmcman.free()
          cowmcman match {
            case Some(wmc) ⇒ wmc.free()
            case _ ⇒
          }
        }

        // Once we've done a round of sampling, should check for entailed literals
        if(mem_oracle != null) {
          val res = checkEntailment(mem_oracle, fg, assignments, sampled, samples)
          // Check if we found anything
          if(res._1.length > 0) {
            // Update lists
            assignments ++= res._3
            sampled :::= res._2
            samples :::= res._1
            compiled --= res._2
            // Condition sdd
            sdd = Compiler.conditionSdd(sdd, res._3.toArray)
            println("here")
          }
        }

        // Get our compiled factor, make assignments and add it
        facSDD(f).ref
        sdd = Compiler.conjoinSDD(sdd, Compiler.conditionSdd(condEvid(facSDD(f), f, fg), assignments.toArray))
        compiledFacs ::= f
        // Add in variables as necessary
        for (v <- f.varNodes) {
          if (!varsAdded.contains(v) && !fg.evidence.contains(v)) {
            sdd = Compiler.conjoinSDD(sdd, Compiler.compile(v.getCNF))
            compiled = compiled + v
            varsAdded = varsAdded + v
          }
        }
      }
      
      // Compute denominator once per sample
      var wmcman: WmcManager = new WmcManager(sdd, true)
      wmcman = Compiler.setWeights(wmcman, fg, true)
      val norm = time { wmcman.propagate() }
      dens.append(Math.log(0.5) * sampled.map(_.n).sum +norm - q)
      //println(Math.log(0.5)*sampled.length)
      //println(norm - Math.log(q))
      // Compute numerator for each function for each sample
      for(i ← 0 to functions.length - 1) {
        if (isCompatible(sampled, samples, functions(i))) {
          Compiler.setFunction(wmcman, compiled.toList, functions(i))
          val prob = time { wmcman.propagate() }
          margs += Math.exp(prob - norm)
          // nums(i).append(wmcman.propagate() - norm + dens(dens.length - 1))
          nums(i).append(prob - norm + dens(dens.length - 1))
        }
      }
      // For collecting statistics
      finalNumSamp.append(sampled.size)
      sampledVars.append(sampled)
      if(checkProp) {
        trueMarg.append(trueMargIter)
        propMarg.append(propMargIter)
      }
      wmcman.free
      sdd.deref
      comparsdd match {
        case Some(sdd) ⇒ sdd.deref()
        case _ ⇒
      }
      Compiler.garbageCollect
    }
    if(preComp == null) {
      preSdd.deref
      for(f ← facSDD.values) {
        f.deref
      }
    }
    Compiler.garbageCollect
    // Compute marginal probability of each variable being sampled
    //val qMargs: Map[VarNode, Double] = (sampledCount.keys zip sampledCount.values.map(_.toDouble / dens.length)).toMap
    //println(qMargs)
    // Update numerators and denominators with product of marginals
    //val corDens: List[Double] = (dens.toList zip sampledVars).map({case (d, vars) ⇒ d - vars.map(v ⇒ Math.log(qMargs(v))).sum})
    //val corNums: Array[List[Double]] = nums.map(fnums ⇒ (fnums.toList zip sampledVars).map({case (d, vars) ⇒ d - vars.map(v ⇒ Math.log(qMargs(v))).sum}))
    //val den: Double = logSumExp(corDens)
    val den: Double = logSumExp(dens)
    //println("Actual number of samples: " + dens.length)
    val ess: Double = Math.exp(2.0*logSumExp(dens) - logSumExp(dens.map(x ⇒ 2.0*x)))
    //println("Effective sample size: ", ess)
    //println("Most variables sampled: ", finalNumSamp.reduceLeft(_ max _))
    //println("Fewest variables sampled: ", finalNumSamp.reduceLeft(_ min _))
    //println("Average variables sampled: ", finalNumSamp.sum.toFloat/finalNumSamp.length)
    if(checkProp) {
      // Write marginals to files
      val truefw = new BufferedWriter(new FileWriter(new File(res + "truemargs.txt")))
      trueMarg.map(l ⇒ truefw.write(l.map(v ⇒ v.toString()).mkString(",") + "\n"))
      truefw.close()

      val propfw = new BufferedWriter(new FileWriter(new File(res + "propmargs.txt")))
      propMarg.map(l ⇒ propfw.write(l.map(v ⇒ v.toString()).mkString(",") + "\n"))
      propfw.close()
    }
    ((sampledCount.keys.toList.map(k ⇒ fg.vars.indexOf(k))) zip sampledCount.values.toList).sortBy({case (i,c) ⇒ c}).reverse.foreach({case (i,c) ⇒ println("Variable ", i, " was sampled ", c, " times.")})
    //println(facOrder.map(_.size))
    //println(facOrder.map(f ⇒ fg.factors.indexOf(f)))
    // Let's try not to pollute the buffer too much
    if(functions.length < 10) {
      println(margs)
    }
    if(compZ) {
      println(f"The normalization constant Z is ${Math.exp(den - Math.log(dens.length))}%5.5f")
      println(dens.map(d ⇒ Math.exp(d)))
      return Array(Math.exp(den - Math.log(dens.length)))
    }
    nums.map(x ⇒ Math.exp(logSumExp(x) - den))
    //nums.map(fnums ⇒ Math.exp(logSumExp((fnums zip dens).map({case (n, d) ⇒ n-d})) - Math.log(dens.length)))
  }


  

  // Create SDD which is the result of conditioning on evidence contained in a factor
  private def condEvid(sdd: Sdd, f: FactorNode, fg: FactorGraph): Sdd = {
    var toCond: Array[Long] = Array[Long]()
    for(v ← f.varNodes) {
      if(fg.evidence.contains(v)) {
        for(i ← List.range(0, v.n)) {
          if(fg.evidence(v) == i) {
            toCond +:= v.sddIDs(i).toLong
            println("conditioning on evidence")
          } else {
            toCond +:= -v.sddIDs(i).toLong
          }
        }
      }
    }
    Compiler.conditionSdd(sdd, toCond)
  }

  // Given a variable, merge all of the factors containing that variable (by conjoining the corresponding SDDs)
  private def mergeFacsVar(sdds: List[Sdd], facs: List[List[FactorNode]], v: VarNode, varmap: Map[VarNode, ListBuffer[FactorNode]]): (List[Sdd], List[List[FactorNode]]) = {
    // We'll take any facs involving this variable
    val toMerge: List[FactorNode] = varmap(v).toList
    // If there's only one, just leave it
    if(toMerge.length <= 1) {
      (sdds, facs)
    } else {
      var retsdds = sdds
      var retfacs = facs
      for(i ← List.range(1, toMerge.length)) {
        val res = mergeFacs(retsdds, retfacs, toMerge(0), toMerge(i))
        retsdds = res._1
        retfacs = res._2
      }
      (retsdds, retfacs)
    }
  }

  private def mergeFacs(sdds: List[Sdd], facs: List[List[FactorNode]], fac1: FactorNode, fac2: FactorNode): (List[Sdd], List[List[FactorNode]]) = {
    val ind1 = facs.indexWhere(_.contains(fac1))
    val ind2 = facs.indexWhere(_.contains(fac2))
    // If they're already in the same SDD, we're done
    if(ind1 == ind2) {
      return (sdds, facs)
    }
    val retsdd = sdds.updated(ind1, Compiler.conjoinSDD(sdds(ind1), sdds(ind2))).patch(ind2, Nil, 1)
    val retfacs = facs.updated(ind1, facs(ind1) ::: facs(ind2)).patch(ind2, Nil, 1)
    (retsdd, retfacs)
  }

  // Example factor ordering, increasing size of factors
  def facOrderingIncSize(fg: FactorGraph): List[FactorNode] = {
    fg.factors.sortBy(_.size)
  }

  // Compute distance between 2 nodes on graph
  private def getDistance(x: VarNode, y: VarNode, fg: FactorGraph): Int = {
    if(x == y) return 0
    val varmap = fg.getVarMap
    var distances = Map[FactorNode, Int]()
    val varvis = Set[VarNode]()
    val facvis = Set[FactorNode]()
    val ret = ArrayBuffer[FactorNode]()
    varvis.add(x)
    varmap(x).map(f ⇒ ret.append(f))
    varmap(x).map(f ⇒ facvis.add(f))
    varmap(x).map(f ⇒ distances += (f → 1))
    var ind = 0
    while(true) {
      for(v ← ret(ind).varNodes.filterNot(v ⇒ varvis.contains(v))) {
        if(v == y) {
          return distances(ret(ind))
        }
        varvis.add(v)
        for(f ← varmap(v).filterNot(f ⇒ facvis.contains(f))) {
          facvis.add(f)
          ret.append(f)
          distances += (f → (distances(ret(ind))+1))
        }
      }
      ind += 1
    }
    -1
  }

  // Factor ordering by BFS from a random variable
  def facOrderingRandomBreadthFirst(fg: FactorGraph): List[FactorNode] = {
    val x = fg.vars(random.nextInt(fg.vars.length))
    facOrderingBreadthFirst(x)(fg)
  }
  
  // Factor ordering by BFS out from source
  // TODO: Make sure this is correct
  def facOrderingBreadthFirst(x: VarNode)(fg: FactorGraph): List[FactorNode] = {
    val varmap = fg.getVarMap
    val varvis = Set[VarNode]()
    val facvis = Set[FactorNode]()
    val ret = ArrayBuffer[FactorNode]()
    varvis.add(x)
    varmap(x).map(f ⇒ ret.append(f))
    varmap(x).map(f ⇒ facvis.add(f))
    var ind = 0
    while(ind < ret.length) {
      for(v ← ret(ind).varNodes.filterNot(v ⇒ varvis.contains(v))) {
        varvis.add(v)
        for(f ← varmap(v).filterNot(f ⇒ facvis.contains(f))) {
          facvis.add(f)
          ret.append(f)
        }
      }
      ind += 1
    }
    println(ret.length)
    println(facvis.size)
    ret.toList
  }

    // Factor ordering by BFS out from source, and then reverse the entire order
  def facOrderingRevBreadthFirst(x: VarNode)(fg: FactorGraph): List[FactorNode] = {
    val varmap = fg.getVarMap
    val varvis = Set[VarNode]()
    val facvis = Set[FactorNode]()
    val ret = ArrayBuffer[FactorNode]()
    varvis.add(x)
    varmap(x).map(f ⇒ ret.append(f))
    varmap(x).map(f ⇒ facvis.add(f))
    var ind = 0
    while(ind < ret.length) {
      for(v ← ret(ind).varNodes.filterNot(v ⇒ varvis.contains(v))) {
        varvis.add(v)
        for(f ← varmap(v).filterNot(f ⇒ facvis.contains(f))) {
          facvis.add(f)
          ret.append(f)
        }
      }
      ind += 1
    }
    println(ret.length)
    println(facvis.size)
    ret.toList.reverse
  }

  // Factor ordering using a postorder traversal of vtree, with factors/variables placed on the vtree based on LCA of all literals
  def facOrderingLCA(v: Vtree)(fg: FactorGraph): List[FactorNode] = {
    println(fg)
    val ret = fg match {
      case fg: CFFactorGraph ⇒
        clearVtreeData(v)
        println(fg)
        fg.factors.map{f ⇒ addDataLCA(f, fg.getFacSDDs(f))}
        postOrderVtree(v)
      case _ ⇒
        println("something went wrong")
        List[FactorNode]()
    }
    ret
  }

  private def addDataLCA(fn: FactorNode, sdd: Sdd): Unit = {
    val lca = Compiler.getManager(0).getLcaOfLiterals(sdd.getUsedVariables.toArray().map(_.asInstanceOf[Long]))
    // println(fn)
    // println(fn :: lca.getData().asInstanceOf[List[FactorNode]])
    lca.setData(fn :: lca.getData().asInstanceOf[List[FactorNode]])
    // println(lca.getData())
  }

  private def clearVtreeData(v: Vtree): Unit = {
    if(v == null) {
      return
    }
    v.setData(List[FactorNode]())
    // println(v)
    // println(v.getData)
    clearVtreeData(v.getLeftChild)
    clearVtreeData(v.getRightChild)
  }

  private def postOrderVtree(v: Vtree) : List[FactorNode] = {
    // println(v)
    // println(v.getData)
    val parent = v.getData.asInstanceOf[List[FactorNode]]
    // println(parent)
    if(v.isLeaf) {
      parent
    } else if(v.getLeftChild != null && v.getRightChild != null) {
      val ret = postOrderVtree(v.getLeftChild) ::: postOrderVtree(v.getRightChild) ::: parent
      ret
    } else if(v.getLeftChild != null) {
      postOrderVtree(v.getLeftChild) ::: parent
    } else {
      postOrderVtree(v.getRightChild) ::: parent
    }
  }


  // Pick the variable based on maximum entropy
  def nextVarEntropy(x: VarNode)(sdd: Sdd, fg: FactorGraph, compiled: List[VarNode], compiledFacs: Set[FactorNode]): VarNode = {
    val comp = compiled.filter(_!= x)
    val wmcman: WmcManager = Compiler.setWeights(new WmcManager(sdd, true), fg, true)
    time { wmcman.propagate() }
    val ret = comp.maxBy(v ⇒ compEntropy(v, wmcman))
    wmcman.free
    ret
  }

  def compEntropy(y: VarNode, wmcman: WmcManager) {
    -(y.sddIDs.map(id ⇒ wmcman.getProbability(id)).map(lp ⇒ lp * Math.exp(lp))).sum
  }

  // This is just a wrapper for easily getting marginals
  def getMarginalOnlineImportanceSampling(x: VarNode, fg: FactorGraph, maxTime: Long, thresh: Int,
                                          getOrder: (FactorGraph) ⇒ List[FactorNode],
                                          getNextVar: (Sdd, FactorGraph, List[VarNode], Set[FactorNode]) ⇒ VarNode,
                                          getPropSample: (VarNode, FactorGraph, WmcManager, List[VarNode], List[Int]) ⇒ Tuple2[Int, Double],
                                          preComp: (Set[VarNode], List[FactorNode], Sdd) = null,
                                          base: Sdd = null,
                                          res: String = "",
                                          checkProp: Boolean = false,
                                          strictSample: Boolean = false,
                                          checkFirstProp: List[Double] = List(),
                                          compZ: Boolean = false,
                                          mem_oracle: WmcManager = null
  ): Array[Double] = {
    var funcs: List[Map[VarNode, List[Boolean]]] = List()
    // Create appropriate list of function 
    for(j ← 0 to x.n-1) {
      var func: Map[VarNode, List[Boolean]] = Map()
      // Need to add each variable to our function
      for(k ← 0 to fg.vars.length-1) {
        if(x == fg.vars(k)) {
          func += (fg.vars(k) → (for(h ← List.range(0, fg.vars(k).n)) yield h == j))
        } else {
          func += (fg.vars(k) → (for(h ← List.range(0, fg.vars(k).n)) yield true))
        }
      }
      funcs ::= func
    }
    doOnlineImportanceSampling(fg, funcs, maxTime, thresh, getOrder, getNextVar, getPropSample, preComp, base, res=res, checkProp=checkProp, strictSample=strictSample, checkFirstProp=checkFirstProp, compZ = compZ)
  }


  // Dummy method that always picks the same ordering (numerical order of variables)
  def nextVarInd(x: VarNode)(sdd: Sdd, fg: FactorGraph, compiled: List[VarNode], compiledFacs: Set[FactorNode]): VarNode = {
    val comp = compiled.filter(_ != x)
    (comp zip comp.map(x ⇒ fg.vars.indexOf(x))).minBy({case (v, i) ⇒ i})._1
  }

  // Dummy method that picks a random variables
  def nextVarRand(x: VarNode)(sdd: Sdd, fg: FactorGraph, compiled: List[VarNode], compiledFacs: Set[FactorNode]): VarNode = {
    val comp = compiled.filter(_ != x)
    comp(random.nextInt(comp.length))
  }

  // Find the variable which when conditioned on reduces the size of the sdd the most
  // TODO: Is there a faster way to do this?
  def nextVarSizeOnly(x: VarNode)(sdd: Sdd, fg: FactorGraph, compiled: List[VarNode], compiledFacs: Set[FactorNode]): VarNode = {
    var bestvar: VarNode = compiled(0)
    var bestval: Long = 0
    for (c ← compiled) {
      // If we're computing a marginal, don't want to sample out that variable
      if(c != x) {
        val diff = sdd.getSize - sdd.condition(c.sddIDs.toArray.map(_.toLong)).getSize
        if (diff > bestval) {
          bestval = diff
          bestvar = c
        }
      }
    }
    bestvar
  }

  // Pick the next variable from a list based on which has the lowest mutual information with the variable of interest
  def nextVarOneVarMI(x: VarNode)(sdd: Sdd, fg: FactorGraph, compiled: List[VarNode], compiledFacs: Set[FactorNode]): VarNode = {
    // Just in case, we should make sure that x isn't in our list of choices
    val comp = compiled.filter(_ != x)
    // Compute marginals
    var wmcman: WmcManager = Compiler.setWeights(new WmcManager(sdd, true), fg, true)
    time { wmcman.propagate() }
    val marginals: List[List[Double]] = comp.map(z ⇒ z.sddIDs.toList.map(y ⇒ Math.exp(wmcman.getProbability(y))))
    val xmars: List[Double] = x.sddIDs.toList.map(y ⇒ Math.exp(wmcman.getProbability(y)))
    // Compute MIs, get min, free stuff
    val debug_comp=singleSourceMI(x, xmars, sdd, fg, comp, marginals)
    // println("here1")
    // println(debug_comp)
    // println(xmars)
    // println(marginals)

    val ret =(singleSourceMI(x, xmars, sdd, fg, comp, marginals) zip comp).minBy({case (m,v) ⇒ m})._2
    wmcman.free
    ret
  }

  // Run floyd-warshall on graph (all edge weights 1)
  def computeDists(fg: FactorGraph): Array[Array[Int]] = {
    val varmap = fg.getVarMap
    // Know that 2*number of variables is more than than the max distance since all edges are 1
    var dp = Array.fill[Array[Int]](fg.vars.length)(Array.fill[Int](fg.vars.length)(fg.vars.length*2))
    // Everything is next to itself
    for(i ← 0 to fg.vars.length-1) {
      dp(i)(i) = 0
    }
    // Consider things to be next to each other if they share a factor
    for(f ← fg.factors) {
      for(i ← 0 to f.varNodes.length - 2) {
        for(j ← i+1 to f.varNodes.length - 1) {
          dp(varIndMap(f.varNodes(i)))(varIndMap(f.varNodes(j))) = 1
        }
      }
    }

    for(k ← 1 to fg.vars.length - 1) {
      for(j ← 1 to fg.vars.length - 1) {
        for(i ← 1 to fg.vars.length - 1) {
          if(dp(i)(j) > dp(i)(k) + dp(k)(j)) {
            dp(i)(j) = dp(i)(k) + dp(k)(j)
          }
        }
      }
    }
    dp
  }

  // Pick next variable based on maximized sum of distances from frontier
  def nextVarMaximizeFrontierDistance(avoid: List[VarNode])(sdd: Sdd, fg: FactorGraph, compiled: List[VarNode], compiledFacs: Set[FactorNode]): VarNode = {
    val varmap = fg.getVarMap
    val allowed = compiled.filterNot(v ⇒ avoid.contains(v))
    // If we've never computed the distances before, let's do it once and then cache
    if(dists == null) {
      println("Precomputing distances")
      dists = computeDists(fg)
      println("Done")
    }
    val frontier: List[VarNode] = compiled.filterNot(v ⇒ varmap(v).forall(f ⇒ compiledFacs.contains(f)))
    // Ensure frontier is non-empty, if it is pick a random element
    if(frontier.length == 0) {
      
      return allowed(random.nextInt(allowed.length))
    }

    val ret = allowed.minBy(v ⇒ frontier.map(fv ⇒ dists(varIndMap(v))(varIndMap(fv))).max)
    ret
  }

  // Pick the next variable based on the RB criteria, but with each frontier variable being treated as the query:
  // That is, for a variable y, with frontier variables f, we'd like to minimize
  // Sum_{x in f} Sum_{y} P(a(x)|y)^2 * P(y)
  // Where a(x) is the event representing the marginal of x

  // We do this by applying bayes rule and minimizing Sum_{Y} P(y|(x))^2/P(y) for each x

  // This is entirely insensitive to the query
  // Also notice that this can not be used with strict sampling
  def nextVarMinimizeFrontierVariance(sdd: Sdd, fg: FactorGraph, compiled: List[VarNode], compiledFacs: Set[FactorNode]): VarNode = {
    val varmap = fg.getVarMap
    val wmcman: WmcManager = Compiler.setWeights(new WmcManager(sdd, true), fg, true)
    time { wmcman.propagate() }
    val frontier: List[VarNode] = compiled.filterNot(v ⇒ varmap(v).forall(f ⇒ compiledFacs.contains(f)))
    frontier.map(_ ⇒ sdd.ref())
    val condSDDs = frontier.map(v ⇒ Compiler.conditionSdd(sdd, Array(v.sddIDs(0))))
    val condwmcs = condSDDs.map(s ⇒ Compiler.setWeights(new WmcManager(s, true), fg, true))
    time { condwmcs.map(_.propagate()) }
    // val frontiervariances = frontier.map(y ⇒ (condwmcs zip frontier).map({case (w, v) ⇒ compRBVarTerm(y, v, wmcman, w)}).max)
    // val innervariances = compiled.filterNot(x ⇒ frontier.contains(x)).map(y ⇒ (condwmcs zip frontier).map({case (w, v) ⇒ compRBVarTerm(y, v, wmcman, w)}).max)
    // println("Variances for frontier terms:")
    // frontiervariances.foreach(println)
    // println("Variances for inner terms: ")
    // innervariances.foreach(println)
    val ret = compiled.minBy(y ⇒ (condwmcs zip frontier).map({case (w, v) ⇒ compRBVarTerm(y, v, wmcman, w)}).max)
    wmcman.free()
    condSDDs.map(_.deref())
    condwmcs.map(_.free())
    ret
  }

  // Pick the next variable based on our criteria:
  // Minimize the variance of the RB sampler directly
  // That is, for an event a and a variable y, minimize
  // Sum_{y} P(a|y)^2 * P(y)

  // We do this by applying bayes rule and minimizing Sum_{Y} P(y|a)^2 / P(y)

  // Notice that this is sensitive to the query, but in a completely general way
  def nextVarMinimizeRBVariance(x: VarNode)(sdd: Sdd, fg: FactorGraph, compiled: List[VarNode], compiledFacs: Set[FactorNode]): VarNode = {
    // Just in case, we should make sure that x isn't in our list of choices ← This shouldn't actually be necessary, query variable should never be chosen
    val comp = compiled.filter(_ != x)
    sdd.ref
    val condSDD = Compiler.conditionSdd(sdd, Array(x.sddIDs(0)))
    val wmcman: WmcManager = Compiler.setWeights(new WmcManager(sdd, true), fg, true)
    val condWmcman = Compiler.setWeights(new WmcManager(condSDD, true), fg, true)
    time { wmcman.propagate() }
    time { condWmcman.propagate() }
    val ret = comp.minBy(y ⇒ compRBVarTerm(y, x, wmcman, condWmcman))
    wmcman.free()
    condSDD.deref()
    condWmcman.free()
    ret
  }

  private def compRBVarTerm(y: VarNode, x: VarNode, wmcman: WmcManager, condWmcman: WmcManager): Double = {
    Math.exp(2*wmcman.getProbability(x.sddIDs(0))) * (y.sddIDs.map(i ⇒ Math.exp(2 * condWmcman.getProbability(i) - wmcman.getProbability(i))).sum - 1)
  }

  
  // Pick the next variable based on max distance from frontier (for reverse BFS)
  def nextVarSourceDist(x: VarNode)(sdd: Sdd, fg: FactorGraph, compiled: List[VarNode], compiledFacs: Set[FactorNode]): VarNode = {
    val comp = compiled.toSet
    // if(comp.contains(x)) {
    //   return x
    // }
    val varmap = fg.getVarMap
    val varvis = Set[VarNode]()
    val facvis = Set[FactorNode]()
    val queue = ArrayBuffer[VarNode]()
    varvis.add(x)
    queue.append(x)
    var ind = 0
    while(ind < queue.length) {
      for(f ← varmap(queue(ind)).filterNot(x ⇒ facvis.contains(x))) {
        facvis.add(f)
        for(v ← f.varNodes.filterNot(v ⇒ varvis.contains(v))) {
          queue.append(v)
          varvis.add(v)
        }
      }
      ind += 1
    }
    queue(queue.lastIndexWhere(v ⇒ compiled.contains(v)))
  }




  // Compute all pairwise MIs with one variable, note marginals should not be in the log domain
  private def singleSourceMI(x: VarNode, xmars: List[Double], sdd: Sdd, fg: FactorGraph, compiled: List[VarNode], margs: List[List[Double]]): List[Double] = {
    // SDDs conditioned on each different setting of x
    val condSDDs: List[Sdd] = x.sddIDs.indices.map(i ⇒ genSDD(x, i, sdd)).toList
    // Construct a WmcManager for each SDD and propagate
    val condWMCs: List[WmcManager] = condSDDs.map(sdd ⇒ Compiler.setWeights(new WmcManager(sdd, true), fg, true))
    time { condWMCs.map(_.propagate) }
    // Get MIs
    val ret = (compiled zip margs).map({case (y, ymars) ⇒ computeMI(x, xmars, y, ymars, condWMCs)})
    // Free stuff
    condSDDs.map(_.deref)
    condWMCs.map(_.free)
    ret
  }

  // Compute the mutual information between x and y, note marginals should not be in the log domain
  private def computeMI(x: VarNode, xmars: List[Double], y: VarNode, ymars: List[Double], condWMCs: List[WmcManager]): Double = {
    var mi: Double = 0
    for(i ← 0 to x.n-1) {
      for(j ← 0 to y.n-1) {
        mi += Math.exp(condWMCs(i).getProbability(y.sddIDs(j))) * xmars(i) * (condWMCs(i).getProbability(y.sddIDs(j)) - Math.log(ymars(j)))
      }
    }
    mi
  }

  // Generates an SDD conditioned on given state of given variable
  private def genSDD(x: VarNode, state: Int, sdd: Sdd): Sdd = {
    sdd.ref
    Compiler.conditionSdd(sdd, x.sddIDs.indices.map(i ⇒ if(i == state) x.sddIDs(i) else -x.sddIDs(i)).map(_.toLong).toArray)
  }

  // Compute marginal by the current marginal in SDD
  // Returns a sample as well as its probability
  // Assumes wmcmanager is in log mode
  // Asserts that the sample + partial assignment is also a member of the deterministic SDD given up front
  def getSDDMargMember(detWmc: WmcManager)(v: VarNode, fg: FactorGraph, wmcman: WmcManager, sampled: List[VarNode], samples: List[Int]): Tuple2[Int, Double] = {
    var probs: ListBuffer[Double] = v.sddIDs.toList.map(x ⇒ Math.exp(wmcman.getProbability(x))).to[ListBuffer]
    if(probs.exists(_.isNaN)) {
      println(probs)
      println("Wmc 0 found")
      return Tuple2(0, 1.0)
    }
    // Control for membership -  do this by renormalizing only the valid assignments
    var basefunc = Map[VarNode, List[Boolean]]()
    // Add in the assignments first
    for((w, i) ← sampled zip samples) {
      basefunc += (w → (for(h ← List.range(0, w.n)) yield h == i))
    }
    // Add in variables that are unassigned/not query
    for(w ← fg.vars) {
      if(w != v && !basefunc.contains(w)) {
        basefunc += (w → (for(h ← List.range(0, w.n)) yield true))
      }
    }
    var mod_detWmc = detWmc
    // For each query we'll generate a function, and if the resulting WMC gives 0 then we'll 0 out the probability
    for(i ← List.range(0, v.n)) {
      val func = basefunc + (v → (for (h ← List.range(0, v.n)) yield h == i))
      mod_detWmc = Compiler.setFunction(mod_detWmc, fg, func)
      if(time { detWmc.propagate() } == detWmc.getZeroWeight) {
        probs(i) = 0
      }
    }

    // Renormalize
    probs = probs.map(d ⇒ d/probs.sum)

    // Sample
    var draw = random.nextDouble()
    var i = -1
    while(draw > 0) {
      i += 1
      draw -= probs(i)
    }
    Tuple2(i, probs(i))
  }

  // Compute marginal by the current marginal in SDD
  // Returns a sample as well as its probability
  // Assumes wmcmanager is in log mode
  def getSDDMarg(v: VarNode, fg: FactorGraph, wmcman: WmcManager, sampled: List[VarNode], samples: List[Int]): Tuple2[Int, Double] = {
    val probs: List[Double] = v.sddIDs.toList.map(x ⇒ Math.exp(wmcman.getProbability(x)))
    if(probs.exists(_.isNaN)) {
      println(probs)
      println("Wmc 0 found")
      return Tuple2(0, 1.0)
    }
    var draw = random.nextDouble()
    var i = -1
    while(draw > 0) {
      i += 1
      draw -= probs(i)
    }
    Tuple2(i, probs(i))
  }

  // Dummy proposal distribution, just gives 1/n where is the number of states the variable can take
  def getSimpleMarg(v: VarNode, fg: FactorGraph, wmcman: WmcManager, sampled: List[VarNode], samples: List[Int]): Tuple2[Int, Double] = {
    Tuple2(random.nextInt(v.n), 1.0/v.n)

  }

  // Dummy proposal distribution, just gives 1/n where is the number of states the variable can take. Also uses membership oracle
  def getSimpleMargMember(detWmc: WmcManager)(v: VarNode, fg: FactorGraph, wmcman: WmcManager, sampled: List[VarNode], samples: List[Int]): Tuple2[Int, Double] = {
    Tuple2(random.nextInt(v.n), 1.0/v.n)
    var probs: ListBuffer[Double] = v.sddIDs.toList.map(_ ⇒ 1.0/v.n).to[ListBuffer]
    // Control for membership -  do this by renormalizing only the valid assignments
    var basefunc = Map[VarNode, List[Boolean]]()
    // Add in the assignments first
    for((w, i) ← sampled zip samples) {
      basefunc += (w → (for(h ← List.range(0, w.n)) yield h == i))
    }
    // Add in variables that are unassigned/not query
    for(w ← fg.vars) {
      if(w != v && !basefunc.contains(w)) {
        basefunc += (w → (for(h ← List.range(0, w.n)) yield true))
      }
    }
    var mod_detWmc = detWmc
    // For each query we'll generate a function, and if the resulting WMC gives 0 then we'll 0 out the probability
    for(i ← List.range(0, v.n)) {
      val func = basefunc + (v → (for (h ← List.range(0, v.n)) yield h == i))
      mod_detWmc = Compiler.setFunction(mod_detWmc, fg, func)
      if(time { detWmc.propagate() } == detWmc.getZeroWeight) {
        probs(i) = 0
      }
    }

    // Renormalize
    probs = probs.map(d ⇒ d/probs.sum)

    // Sample
    var draw = random.nextDouble()
    var i = -1
    while(draw > 0) {
      i += 1
      draw -= probs(i)
    }
    Tuple2(i, probs(i))
  }

  def getGivenMarg(prop: Map[VarNode, List[Double]])(v: VarNode, fg: FactorGraph, wmcman: WmcManager, sampled: List[VarNode], samples: List[Int]): Tuple2[Int, Double] = {
    val probs = prop(v)
    if(probs.exists(_.isNaN)) {
      println(probs)
      println("Wmc 0 found")
      return Tuple2(0, 1.0)
    }
    var draw = random.nextDouble()
    var i = -1
    while(draw > 0) {
      i += 1
      draw -= probs(i)
    }
    Tuple2(i, probs(i))
  }

  def getGivenMargMember(detWmc: WmcManager, prop: Map[VarNode, List[Double]])(v: VarNode, fg: FactorGraph, wmcman: WmcManager, sampled: List[VarNode], samples: List[Int]): Tuple2[Int, Double] = {
    var probs = prop(v).to[ListBuffer]
    // Control for membership -  do this by renormalizing only the valid assignments
    var basefunc = Map[VarNode, List[Boolean]]()
    // Add in the assignments first
    for((w, i) ← sampled zip samples) {
      basefunc += (w → (for(h ← List.range(0, w.n)) yield h == i))
    }
    // Add in variables that are unassigned/not query
    for(w ← fg.vars) {
      if(w != v && !basefunc.contains(w)) {
        basefunc += (w → (for(h ← List.range(0, w.n)) yield true))
      }
    }
    var mod_detWmc = detWmc
    // For each query we'll generate a function, and if the resulting WMC gives 0 then we'll 0 out the probability
    for(i ← List.range(0, v.n)) {
      val func = basefunc + (v → (for (h ← List.range(0, v.n)) yield h == i))
      mod_detWmc = Compiler.setFunction(mod_detWmc, fg, func)
      if(time { detWmc.propagate() } == detWmc.getZeroWeight) {
        probs(i) = 0
      }
    }

    // Renormalize
    probs = probs.map(d ⇒ d/probs.sum)

    // Sample
    var draw = random.nextDouble()
    var i = -1
    while(draw > 0) {
      i += 1
      draw -= probs(i)
    }
    Tuple2(i, probs(i))
  }


  // Computes whether or not a list of sampled variables is compatible with a function
  def isCompatible(sampled: List[VarNode], samples: List[Int], function: Map[VarNode, List[Boolean]]): Boolean = {
    (sampled zip samples).forall({ case (v: VarNode, y: Int) => function(v)(y)})
  }

  // Standard logsumexp implementation - does this exist somewhere in scala?
  def logSumExp(a: Double, b: Double) = {
    if(a == Double.NegativeInfinity) b
    else if (b == Double.NegativeInfinity) a
    else if(a < b) b + Math.log(1 + Math.exp(a-b))
    else a + Math.log(1+Math.exp(b-a));
  }

  // Standard logsumexp implementation - does this exist somewhere in scala?
  def logSumExp(a:Seq[Double]):Double = {
    a.length match {
      case 0 => Double.NegativeInfinity;
      case 1 => a(0)
      case 2 => logSumExp(a(0),a(1));
      case _ =>
        val m = a reduceLeft(_ max _);
        if(m.isInfinite) {
          m
        }
        else {
          var i = 0;
          var accum = 0.0;
          while(i < a.length) {
            accum += Math.exp(a(i) - m);
            i += 1;
          }
          m + Math.log(accum);
        }
    }
  }

  def checkEntailment(wmcman: WmcManager, fg: FactorGraph, assignments: ArrayBuffer[Long], sampled: List[VarNode], samples: List[Int]): (List[Int], List[VarNode], ArrayBuffer[Long]) = {
    val setassn = Set(assignments: _*)

    var basefunc = Map[VarNode, List[Boolean]]()
    // Add in the assignments first
    for((w, i) ← sampled zip samples) {
      basefunc += (w → (for(h ← List.range(0, w.n)) yield h == i))
    }
    // Add in variables that are unassigned
    for(w ← fg.vars) {
      if(!basefunc.contains(w) ) {
        basefunc += (w → (for(h ← List.range(0, w.n)) yield true))
      }
    }
    var mod_wmc = Compiler.setFunction(wmcman, fg, basefunc)
    val wmc = time { mod_wmc.propagate() }
    var entailed_vars = List[VarNode]()
    var entailed_vals = List[Int]()
    var new_assignments = ArrayBuffer[Long]()
    for(v ← fg.vars) {
      // Dont' need to check evidence
      if(!fg.evidence.contains(v)) {
        for(i ← v.sddIDs.indices) {
          if(!setassn.contains(v.sddIDs(i)) && !setassn.contains(-v.sddIDs(i))) {
            if(mod_wmc.getProbability(v.sddIDs(i)) == mod_wmc.getOneWeight) {
              entailed_vars ::= v
              entailed_vals ::= i
              for(j ← 0 to v.n - 1) {
                if(j == i) {
                  new_assignments.append(v.sddIDs(j))
                } else {
                  new_assignments.append(-v.sddIDs(j))
                }
              }
            }
          }
        }
      }
    }
    (entailed_vals, entailed_vars, assignments)
  }
}

