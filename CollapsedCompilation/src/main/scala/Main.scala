
/**
  */
import sdd.{Sdd, WmcManager, Vtree, SddManager}
import scopt.OptionParser
import java.io.File
import scala.io.Source
import scala.collection.mutable.ListBuffer

case class Config(time: Int = -1, size: Int = -1, litmap: String = "", uai: String = "", vtree: String = "", sddDir: String = "", baseCnf: String = "", baseSdd: String = "", heuristic: String = "mi", evidence: String = "", query_var: Int = 0, checkProp: Boolean = false, strictSampling: Boolean = false, trueMarg: Double = -1, order: String="bfs", mem_oracle: Boolean = false, base_vtree: String = "", check_entailment: Boolean = false, dummy: Boolean = false, wcnf: String = "", weights: String = "", prop_dist: String = "")

object Main {
  def main(args: Array[String]): Unit = {
    val parser = new OptionParser[Config]("Online Collapsed Importance Sampler") {
      opt[String]('l', "litmap").valueName("<file>").action((x,c) ⇒ c.copy(litmap = x)).text("litmap is a required file for compiled mode")
      opt[String]('u', "uai").required().valueName("<file>").action((x,c) ⇒ c.copy(uai = x)).text("A uai file is always required")
      opt[String]('d', "sddDir").valueName("<folder>").action((x, c) ⇒ c.copy(sddDir=x)).text("Directory where sdd files are, required for compiled mode")
      opt[String]('v', "vtree").valueName("<file>").action((x, c) ⇒ c.copy(vtree=x)).text("An initial vtree file is required for compiled mode, optional otherwise")
      opt[Int]('t', "time").required().valueName("<value>").action((x,c) ⇒ c.copy(time=x)).text("Amount of time to run sampler for - this is required")
      opt[Int]('s', "size").required().valueName("<value>").action((x,c) ⇒ c.copy(size=x)).text("Maximum size of SDD to allow during sampling - this is required")
      // opt[String]('r', "resLoc").valueName("<string>").action((x,c) ⇒ c.copy(res=x)).text("Directory and prefix for results files. Default is empty string (i.e. put stuff in current directory with no prefix)")
      opt[String]('b', "baseCNF").valueName("<file>").action((x,c) ⇒ c.copy(baseCnf=x)).text("Optional base CNF to start all compilation iterations from")
      opt[String]('B', "baseSDD").valueName("<file>").action((x,c) ⇒ c.copy(baseSdd=x)).text("Optional base SDD to start all compilation iterations from (will prefer this or base sdd)")
      opt[String]('H', "heuristic").valueName("<string>").action((x,c) ⇒ c.copy(heuristic=x)).text("""Heuristic for selecting the next variable:
mi for mutual information
rand for random
size for minimizing size
ind for index
fd for max distance from frontier
ent for minimum entropy""")
      opt[String]('O', "order").valueName("<string>").action((x,c) ⇒ c.copy(order=x)).text("""Order in which to compile factors:
bfs for breadth first from query (default)
rev for reverse breadth first from query
rbfs for bfs beginning from a random variable at each sample
vto for order based on vtree
""")
      opt[String]('e', "evidence").action((x, c) ⇒ c.copy(evidence=x)).text("File containing evidence")
      opt[Int]('q', "query_variable").action((x, c) ⇒ c.copy(query_var=x)).text("Which variable to get the marginal for. Use -1 for the last variable")
      // opt[Unit]("check_proposal").action((_, c) ⇒ c.copy(checkProp=true)).text("Generate files with the true conditional marginals at each step compared to those given by the proposal distribution")
      // opt[Unit]('S', "strict_sampling").action((_, c) ⇒ c.copy(strictSampling=true)).text("Where possible, select next variable to condition from the set of variables whose factors are fully compiled already.")
      // opt[String]("check_firstprop").valueName("<file>").action((x, c) ⇒ c.copy(checkFirstProp=x)).text("Set of correct marginals to compare proposal for first variable to")
      // opt[Double]("true_marg").valueName("<value>").action((x, c) ⇒ c.copy(trueMarg=x)).text("True marginal to compare result with")
      // opt[Unit]("prob_evid").action((_,c) ⇒ c.copy(prEvid=true)).text("Also approximate the partition function Z")
      // opt[Unit]("all_margs").action((_,c) ⇒ c.copy(allMarg=true)).text("Compute all marginals using one set of samples and random starting points for BFS")
      opt[Unit]('M', "member_oracle").action((_,c) ⇒ c.copy(mem_oracle=true)).text("Use a second SDD consisting of the deterministic base + evidence as an oracle for memebership rather than precompiling the base")
      opt[String]('V', "base_vtree").valueName("<file>").action((x,c) ⇒ c.copy(base_vtree=x)).text("The vtree to use for the base CNF/SDD (default is same vtree as factorgraph)")
      opt[Unit]('E', "check_entailment").action((_,c) ⇒ c.copy(check_entailment=true)).text("After you finish conditioning on variables until SDD size falls below threshold, check for any entailed literals using the membership oracle. Must be using the membership oracle as well.")
      // opt[Unit]('D', "dummy_proposal").action((_, c) ⇒ c.copy(dummy=true)).text("Use a dummy uniform proposal distribution")
      // opt[String]("wcnf").valueName("<file>").action((x,c) ⇒ c.copy(wcnf=x)).text("Use this option to do approximate weighted model counting on a CNF. Requires also using the weights flag.")
      // opt[String]("weights").valueName("<file>").action((x,c) ⇒ c.copy(weights=x)).text("File with weights for each literal that's weighted, as well as any queries.")
      // opt[String]("prop_file").valueName("<file>").action((x,c) ⇒ c.copy(prop_dist=x)).text("File with a marginal distribution for each variable to use as a proposal.")

    }
    parser.parse(args, Config()) match {
      case Some(config) ⇒
        // Config variables we removed
        val checkFirstProp = ""
        val allMarg = false
        val prEvid = false

        println("Config worked fine")
        if(config.wcnf != "") {
          val cnf = CNF.fromFile(config.wcnf)
          val lines = Source.fromFile(config.weights).getLines.toList
          var lits: ListBuffer[Int] = ListBuffer()
          var litweights: ListBuffer[Double] = ListBuffer()
          var queries: ListBuffer[Int] = ListBuffer()
          for(line ← lines) {
            val tokens = line.split("\\s+").toList
            if(tokens(0) == "q") {
              queries += tokens(1).toInt
            } else {
              lits += tokens(0).toInt
              lits += -tokens(0).toInt
              litweights += tokens(1).toFloat
              litweights += 1.0 - tokens(1).toFloat
            }
          }

          if(config.vtree != "") {
            Compiler.setVtree(Vtree.read(config.vtree))
          } else {
            Compiler.getManager(cnf.max)
          }
          //CNFSampler.doOnlineImportanceSampling(cnf, queries.toList, System.currentTimeMillis()/1000 + config.time, config.size, CNFSampler.orderVtreeLCA(Compiler.getManager(0).getVtree), CNFSampler.nextVarEntropy(queries.toList), CNFSampler.getSDDMarg, (lits.toList, litweights.toList)).foreach(println)
          // CNFSampler.doOnlineImportanceSampling(cnf, queries.toList, config.time, config.size, CNFSampler.orderRevBFS(queries(0)), CNFSampler.nextVarEntropy(queries.toList), CNFSampler.getSDDMarg, (lits.toList, litweights.toList)).foreach(println)
        } else {
          // Set Vtree
          Compiler.setVtree(Vtree.read(config.vtree))

          // Load in factorgraph
          val fg = FactorGraph.fromLitmap(config.litmap, config.sddDir, config.uai)
          // Assign query variable
          var qv: Int = config.query_var
          if(qv == -1) {
            qv = fg.vars.length - 1
          }
          // Assign heuristic
          val heur = config.heuristic match {
            case "mi" ⇒ Sampler.nextVarOneVarMI(fg.vars(qv))_;
            case "rand" ⇒ Sampler.nextVarRand(fg.vars(qv))_;
            case "size" ⇒ Sampler.nextVarSizeOnly(fg.vars(qv))_;
            case "ind" ⇒ Sampler.nextVarInd(fg.vars(qv))_;
            case "fd" ⇒ Sampler.nextVarSourceDist(fg.vars(qv))_;
            case "ent" ⇒ Sampler.nextVarEntropy(fg.vars(qv))_;
            case "rbvar" ⇒ Sampler.nextVarMinimizeRBVariance(fg.vars(qv))_;
            case "frb" ⇒
              assert(!config.strictSampling)
              Sampler.nextVarMinimizeFrontierVariance _;
          }

          val order = config.order match {
            case "bfs" ⇒ Sampler.facOrderingBreadthFirst(fg.vars(qv))_;
            case "rev" ⇒ Sampler.facOrderingRevBreadthFirst(fg.vars(qv))_;
            case "rbfs" ⇒ Sampler.facOrderingRandomBreadthFirst _;
            case "vto" ⇒ Sampler.facOrderingLCA(Compiler.getManager(0).getVtree)_;
          }

          // Add evidence
          if(config.evidence != "") {
            fg.evidenceFromFile(config.evidence)
          }

          // Figure out base, if there's no file then null
          var base: Sdd = null
          if(config.baseSdd != "") {
            if(config.base_vtree != "") {
              // If we're not using a membership oracle not allowed to use a different vtree
              if(!config.mem_oracle) {
                println("This is unsupported. To use a different vtree for your base SDD, you must use the member oracle function.")
                System.exit(1)
              } else {
                // If we are, we should construct a new manager from the other vtree
                val base_manager = new SddManager(Vtree.read(config.base_vtree))
                base_manager.useAutoGcMin(false)
                var read = Compiler.readSdd(config.baseSdd, base_manager)
                println("Done reading sdd")
                if(!prEvid) {
                  for(i ← 0 to fg.getEvidCNF.clauses.length - 1) {
                    read = read.conjoin(Compiler.compileDiffManager(new CNF(List(fg.getEvidCNF.clauses(i))), base_manager))
                    println(s"Compiled in evidence $i")
                  }
                }
                base = read
                // base = Compiler.conjoinSDD(Compiler.readSdd(config.baseSdd, base_manager), Compiler.compileDiffManager(fg.getEvidCNF, base_manager))
              }
            } else {
              if(!prEvid) {
                base = Compiler.conjoinSDD(Compiler.readSdd(config.baseSdd, Compiler.getManager(0)), Compiler.compile(fg.getEvidCNF))
              } else {
                base = Compiler.readSdd(config.baseSdd, Compiler.getManager(0))
              }
            }
          } else if(config.baseCnf != "") {
            if(!prEvid) {
              base = Compiler.compile(CNF.fromFile(config.baseCnf).conjoin(fg.getEvidCNF))
            } else {
              base = Compiler.compile(CNF.fromFile(config.baseCnf))
            }
          }

          var oracle_wmc: WmcManager = null
          // If we're using the oracle, better construct WMC for base and generate our proposal function
          val propMarg = if(config.mem_oracle) {
            oracle_wmc = new WmcManager(base, true)
            base = null
            if(config.prop_dist != "") {
              Sampler.getGivenMargMember(oracle_wmc, loadProp(config.prop_dist, fg))_
            } else if(config.dummy) {
              Sampler.getSimpleMargMember(oracle_wmc)_
            } else {
              Sampler.getSDDMargMember(oracle_wmc)_
            }
          } else {
            if(config.prop_dist != "") {
              Sampler.getGivenMarg(loadProp(config.prop_dist, fg))_
            } else if(config.dummy) {
              Sampler.getSimpleMarg _
            } else {
              Sampler.getSDDMarg _
            }
          }

          // Check if we need the oracle wmc
          if(config.check_entailment && oracle_wmc == null) {
            println("Need to enable membership oracle for entailment")
            System.exit(1)
          } else if(!config.check_entailment) {
            oracle_wmc = null
          }
          // True marginals for checking first proposal
          val truemarg = if(checkFirstProp != "") {
            val line = Source.fromFile(checkFirstProp).getLines.toList.mkString(" ")
            val tokens = line.split("\\s+").filter(s ⇒ s != "MAR" && s != "")
            val numVars = tokens(0).toInt
            assert(numVars == fg.vars.length)
            List.range(0, numVars).map(i ⇒ tokens(2 + 3*i).toDouble)
          } else {
            List()
          }

        
          // Probability of evidence
          if(prEvid) {
            // Construct evidence function
            var func: Map[VarNode, List[Boolean]] = Map()
            for(v ← fg.vars) {
              if(!fg.evidence.contains(v)) {
                func += (v → (for(h ← List.range(0, v.n)) yield true))
              } else {
                func += (v → (for(h ← List.range(0, v.n)) yield h == fg.evidence(v)))
              }
            }

            val funcs = List(func)
            // Going to use distances, so need to precompute
            if(Sampler.dists == null) {
              println("Precomputing distances")
              Sampler.dists = Sampler.computeDists(fg)
            }
            val heur = Sampler.nextVarMaximizeFrontierDistance(fg.evidence.keys.toList)_
            // We're treating evidence as a query here, so we actually want our sampling to happen with no evidence
            fg.evidence = Map()
            Sampler.doOnlineImportanceSampling(fg, funcs, config.time, config.size, Sampler.facOrderingRandomBreadthFirst, heur, propMarg, base=base, checkProp=config.checkProp, strictSample=config.strictSampling, mem_oracle=oracle_wmc, doPreComp=false).foreach(println)
            // Sampler.doOnlineImportanceSampling(fg, funcs, config.time, config.size, Sampler.facOrderingRandomBreadthFirst, heur, propMarg, base=base, checkProp=config.checkProp, strictSample=config.strictSampling, mem_oracle=oracle_wmc, doPreComp=false).foreach(println)
          } else if(allMarg) {
            Sampler.allMarginalsOnlineSampling(fg, config.time, config.size, propMarg, base=base).foreach(println)
            // Sampler.allMarginalsOnlineSampling(fg, config.time, config.size, propMarg, base=base).foreach(println)
          } else {
            // Sample
            val res = Sampler.getMarginalOnlineImportanceSampling(fg.vars(qv), fg, config.time, config.size, order, heur, propMarg, base=base, checkProp=config.checkProp, strictSample=config.strictSampling, checkFirstProp=truemarg, mem_oracle=oracle_wmc)
            // val res = Sampler.getMarginalOnlineImportanceSampling(fg.vars(qv), fg, config.time, config.size, order, heur, propMarg, base=base, checkProp=config.checkProp, strictSample=config.strictSampling, checkFirstProp=truemarg, mem_oracle=oracle_wmc)
            res.foreach(println)
            if(config.trueMarg >= 0) {
              println(s"KL divergence under distribution of true query marginal: ${-(config.trueMarg* Math.log(res(1)/config.trueMarg) + (1-config.trueMarg) * Math.log(res(0)/(1-config.trueMarg)))}")
            }
          }
        }
      case None ⇒
        println("ERROR")
    }
  }

  private def loadProp(fname: String, fg: FactorGraph): Map[VarNode, List[Double]] = {
    val line = Source.fromFile(fname).getLines.toList.mkString(" ")
    val entries: Array[String] = line.split("\\s+").filterNot(_ == "")
    var ret: Map[VarNode, List[Double]] = Map()
    assert(entries(0).toInt == fg.vars.length)
    var ind = 1
    var vind = 0
    while(ind < entries.length) {
      val card = entries(ind).toInt
      assert(card == fg.vars(vind).n)
      ret += (fg.vars(vind) → entries.slice(ind+1, ind+1+card).map(_.toDouble).toList)
      vind += 1
      ind += 1 + card
    }

    ret
  }
}
