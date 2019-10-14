/**
 */
import sdd.{ Sdd, SddManager, WmcManager, Vtree }

object Compiler {
  var counter: Int = 0
  var manager: SddManager = null
  def setWeights(wmcman: WmcManager, fg: FactorGraph, log: Boolean = false): WmcManager = {
    fg match {
      case fg: UFFactorGraph ⇒ setFactorWeights(wmcman, fg.factors, log)
      case fg: CFFactorGraph ⇒ setLitWeights(wmcman, fg.facLits, fg.facWeights, log)
    }
  }

  def setLitWeights(wmcman: WmcManager, lits: List[Int], weights: List[Double], log: Boolean = false): WmcManager = {
    if (log) {
      (lits zip weights).map(x ⇒ wmcman.setLiteralWeight(x._1, Math.log(x._2)))
    } else {
      (lits zip weights).map(x ⇒ wmcman.setLiteralWeight(x._1, x._2))
    }
    wmcman
  }

  def setFactorWeights(wmcman: WmcManager, factors: List[FactorNode], log: Boolean = false): WmcManager = {
    if (log) {
      factors.map((x: FactorNode) => (x.contents.values.toList zip x.sddIDs.values.toList).map(x => wmcman.setLiteralWeight(x._2, Math.log(x._1))))
    } else {
      factors.map((x: FactorNode) => (x.contents.values.toList zip x.sddIDs.values.toList).map(x => wmcman.setLiteralWeight(x._2, x._1)))
    }
    wmcman
  }

  def setFunction(wmcman: WmcManager, fg: FactorGraph, function: Map[VarNode, List[Boolean]]): WmcManager = {
    setFunction(wmcman, fg.vars, function)
  }

  def setFunction(wmcman: WmcManager, vars: List[VarNode], function: Map[VarNode, List[Boolean]]): WmcManager = {
    vars.map(x ⇒ (function(x) zip x.sddIDs).map{ case(b, y) ⇒ if (b) wmcman.setLiteralWeight(y, wmcman.getOneWeight) else wmcman.setLiteralWeight(y, wmcman.getZeroWeight)})
    wmcman

  }

  def compile(fg: FactorGraph): Sdd = {
    fg match {
      case fg: UFFactorGraph ⇒ compile(fg.getCNF)
      case fg: CFFactorGraph ⇒ fg.facSDDs.foldLeft(compile(CNF.conjoin(fg.vars.map(x ⇒ x.getCNF))))(conjoinSDD)
    }
  }

  def compileDiffManager(cnf: CNF, manager: SddManager): Sdd = {
    compileCNF(manager, cnf, Map[Long, Boolean]())
  }

  def compile(cnf: CNF): Sdd = {
    compileWithAssignments(cnf, Map[Long, Boolean]())
  }

  def compileWithAssignments(cnf: CNF, assignments: Map[Long, Boolean]): Sdd = {
    compileCNF(getManager(Node.getSDDVarCount), cnf, assignments)
  }

  def compileWithAssignments(cnf: CNF, assignments: List[Long]): Sdd = {
    val ass = (assignments.map(x ⇒ Math.abs(x)) zip assignments.map(x ⇒ x > 0)).toMap
    compileWithAssignments(cnf, ass)
  }

  def readSdd(file: String, manager: SddManager): Sdd = {
    refRet(Sdd.read(file, manager))
  }

  // TODO: Add optimization for compiling CNFS for factors to make it linear instead of quadratic

  private def compileCNF(manager: SddManager, cnf: CNF, assignments: Map[Long, Boolean]): Sdd = {
    // Just compile all clauses and then conjoin them
    cnf.clauses.map(x ⇒ compileClause(manager, x, assignments)).foldLeft(constructValSDD(manager, true))(conjoinSDD(_,_))
  }

  private def compileClause(manager: SddManager, cl: Clause, assignments: Map[Long, Boolean]): Sdd = {
    cl.lits.map(x ⇒ if(assignments.contains(x)) constructValSDD(manager, assignments(x)) else constructLiteralSDD(manager, x)).foldLeft(constructValSDD(manager, false))(disjoinSDD(_,_))
  }

  def constructValSDD(manager: SddManager, ass: Boolean): Sdd = {
    refRet(new Sdd(ass, manager))
  }

  def constructLiteralSDD(manager: SddManager, varID: Int): Sdd = {
    refRet(new Sdd(varID, manager))
  }

  def disjoinSDD(a: Sdd, b: Sdd): Sdd = {
    // All we need to do is disjoin and then update our refs
    val ret: Sdd = a.disjoin(b)
    ret.ref()
    a.deref()
    b.deref()
    return ret
  }

  def conjoinSDD(a: Sdd, b: Sdd): Sdd = {
   // All we need to do is conjoin and then update our refs
    val ret: Sdd = a.conjoin(b)
    ret.ref()
    a.deref()
    b.deref()
    return ret
  }

  def bijectSDD(a: Sdd, b: Sdd): Sdd = {
   /*
    Bijection is a little more complicated
    We'll use the fact that A <=> B is equivalent to
    (A => B) and (B => A)
    Which is also equivalent to
    (~A v B) and (~B v A)
     */

    val nega = refRet(a.negate)
    val negadisb = refRet(nega.disjoin(b))
    val negb = refRet(b.negate)
    val negbdisa = refRet(negb.disjoin(a))
    val ret = refRet(negadisb.conjoin(negbdisa))
    a.deref()
    b.deref()
    nega.deref()
    negadisb.deref()
    negb.deref()
    negbdisa.deref()
    return ret
  }

  def conditionSdd(a: Sdd, lits: Array[Long]): Sdd = {
   // Condition on the value of a literal (positive is true, negative is false)
    val ret = refRet(a.condition(lits))
    a.deref()
    return ret
  }

  private def refRet(a: Sdd): Sdd = {
    a.ref()
    return a
  }

  def getManager(n: Int): SddManager = {
    if (manager == null) {
      manager = new SddManager(n, false)
    }
    return manager
  }

  def setManagerSize(n: Int): SddManager = {
    assert(manager == null)
    manager = new SddManager(n, false)
    return manager
  }

  def setVtree(vtree: Vtree): SddManager = {
    assert(manager == null)
    manager = new SddManager(vtree)
    manager.useAutoGcMin(false)
    return manager
  }

  def garbageCollect = {
    manager.garbageCollect()
  }

  def compileTest(): Unit = {
    val manager: SddManager = new SddManager(2, false)
    val xSDD: Sdd = new Sdd(1, manager)
    val ySDD: Sdd = new Sdd(2, manager)

    val and: Sdd = xSDD.conjoin(ySDD)
    val or: Sdd = xSDD.disjoin(ySDD)

    val orwmc: WmcManager = new WmcManager(or, false)
    println(orwmc.propagate())
    println(orwmc.getProbability(1))
    println(orwmc.getProbability(2))

    val andwmc: WmcManager = new WmcManager(and, false)
    println(andwmc.propagate())
    andwmc.setLiteralWeight(1, 2.0)
    println(andwmc.propagate())
    println(andwmc.getProbability(1))
    println(andwmc.getProbability(2))

  }

  def nosddgridtest(fg: UFFactorGraph): Unit = {
    // getManager(Node.getSDDVarCount)
    var sdd = constructValSDD(getManager(0), true)
    sdd = conjoinSDD(sdd, compile(fg.vars(0).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.vars(1).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.vars(2).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.vars(3).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.vars(4).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.vars(5).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.vars(6).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.vars(8).getCNF))

    sdd = conjoinSDD(sdd, compile(fg.factors(0).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.factors(1).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.factors(2).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.factors(3).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.factors(12).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.factors(13).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.factors(15).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.factors(18).getCNF))
    val wmcman = Compiler.setWeights(new WmcManager(sdd, true), fg, true)
    println(wmcman.propagate())
    println(Math.exp(wmcman.getProbability(1)))

  }

  def cfgtest(cfg: CFFactorGraph): Unit = {
    var sdd = constructValSDD(getManager(0), true)
    val cwmcman = Compiler.setWeights(new WmcManager(sdd, true), cfg, true)
    println(cfg.facWeights zip cfg.facLits)
    println(cwmcman.propagate())
  }

  def ufgtest(ufg: UFFactorGraph): Unit = {
    var sdd = constructValSDD(getManager(0), true)
    val uwmcman = Compiler.setWeights(new WmcManager(sdd, true), ufg, true)
    // val uwmcman = new WmcManager(sdd, true)
    // val weights = ufg.factors.flatMap(x ⇒ x.contents.values)
    // for(i ← 0 to weights.length-1) {
    //   uwmcman.setLiteralWeight(i+33, Math.log(weights(i)))
    // }
    println(ufg.factors.flatMap(x ⇒ x.contents.values) zip ufg.factors.flatMap(x ⇒ x.sddIDs.values))
    println(uwmcman.propagate())

  }
  def cfgufgtest(cfg: CFFactorGraph, ufg: UFFactorGraph): Unit = {
    var sdd = constructValSDD(getManager(0), true)
    val cwmcman = Compiler.setWeights(new WmcManager(sdd, true), cfg, true)
    println(getManager(0).print())
    println(cfg.facWeights zip cfg.facLits)
    println(cwmcman.propagate())
    val uwmcman = Compiler.setWeights(new WmcManager(sdd, true), ufg, true)
    // val uwmcman = new WmcManager(sdd, true)
    // val weights = ufg.factors.flatMap(x ⇒ x.contents.values)
    // for(i ← 0 to weights.length-1) {
    //   uwmcman.setLiteralWeight(i+33, Math.log(weights(i)))
    // }
    println(ufg.factors.flatMap(x ⇒ x.contents.values) zip ufg.factors.flatMap(x ⇒ x.sddIDs.values))
    println(uwmcman.propagate())
  }

  def sddgridtest(fg: CFFactorGraph): Unit = {
    var sdd = constructValSDD(getManager(0), true)
    sdd = conjoinSDD(sdd, compile(fg.vars(0).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.vars(1).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.vars(2).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.vars(3).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.vars(4).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.vars(5).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.vars(6).getCNF))
    sdd = conjoinSDD(sdd, compile(fg.vars(8).getCNF))

    sdd = conjoinSDD(sdd, fg.facSDDs(0))
    sdd = conjoinSDD(sdd, fg.facSDDs(1))
    sdd = conjoinSDD(sdd, fg.facSDDs(2))
    sdd = conjoinSDD(sdd, fg.facSDDs(3))
    sdd = conjoinSDD(sdd, fg.facSDDs(12))
    sdd = conjoinSDD(sdd, fg.facSDDs(13))
    sdd = conjoinSDD(sdd, fg.facSDDs(15))
    sdd = conjoinSDD(sdd, fg.facSDDs(18))

    val wmcman = Compiler.setWeights(new WmcManager(sdd, true), fg, true)
    println(wmcman.propagate())
    println(Math.exp(wmcman.getProbability(5)))
  }

  // Implement partial compilation of grid_test4 for sanity
  def partialgridtest(v: Vtree, fg: FactorGraph): Unit = {
    getManager(48)
    var clauses: List[Clause] = List[Clause]()
    // Indicators
    for(i ← 1 to 8) {
      clauses = clauses ::: constructIndClauses(2*i - 1)
    }
    for(i ← 1 to 8) {
      clauses = clauses ::: constructIndClauses(47 + 2*i)
    }
    // Factors
    clauses = clauses ::: constructPWClause(1, 3, 17)
    clauses = clauses ::: constructPWClause(3, 5, 21)
    clauses = clauses ::: constructPWClause(5, 7, 25)
    clauses = clauses ::: constructPWClause(1, 9, 29)
    clauses = clauses ::: constructPWClause(9, 15, 33)
    clauses = clauses ::: constructPWClause(3, 11, 37)
    clauses = clauses ::: constructPWClause(5, 13, 41)
    clauses = clauses ::: constructPWClause(9, 11, 45)

    var lits: List[Tuple2[Long, Double]] = List[Tuple2[Long, Double]]()
    lits ::= ((17, 0.88047021))
    lits ::= ((18, 0.33679604))
    lits ::= ((19, 0.33279118))
    lits ::= ((20, 0.09045948))
    lits ::= ((21, 0.50488520))
    lits ::= ((22, 0.36876676))
    lits ::= ((23, 0.48665838))
    lits ::= ((24, 0.44855843))
    lits ::= ((25, 0.78073606))
    lits ::= ((26, 0.34333805))
    lits ::= ((27, 0.39859923))
    lits ::= ((28, 0.07149606))
    lits ::= ((29, 0.65879017))
    lits ::= ((30, 0.58063666))
    lits ::= ((31, 0.77224033))
    lits ::= ((32, 0.58590098))
    lits ::= ((33, 0.01446760))
    lits ::= ((34, 0.09547644))
    lits ::= ((35, 0.41765832))
    lits ::= ((36, 0.52853706))
    lits ::= ((37, 0.59900289))
    lits ::= ((38, 0.49333989))
    lits ::= ((39, 0.53065028))
    lits ::= ((40, 0.86804166))
    lits ::= ((41, 0.89350182))
    lits ::= ((42, 0.29807679))
    lits ::= ((43, 0.48419910))
    lits ::= ((44, 0.00030584))
    lits ::= ((45, 0.25186380))
    lits ::= ((46, 0.75658332))
    lits ::= ((47, 0.86623977))
    lits ::= ((48, 0.72783917))
    val sdd = compile(new CNF(clauses))
    var wmcman = new WmcManager(sdd, false)
    setWeights(wmcman, fg, false)
    // lits.map(x ⇒ wmcman.setLiteralWeight(x._1, x._2))
    println(wmcman.propagate())
    println(wmcman.getProbability(5))
    println(sdd.getId)
    println(sdd.getSize)

  }

  def constructIndClauses(start: Int): List[Clause] = {
    new Clause(List(start, start + 1)) :: List(new Clause(List(-start, -start-1)))
  }

  def constructPWClause(ind1: Int, ind2: Int, start: Int): List[Clause] = {
    var clauses: List[Clause] = List[Clause]()
    clauses ::= new Clause(List(-(ind1), -(ind2), start))
    clauses ::= new Clause(List(-start, ind1))
    clauses ::= new Clause(List(-start, ind2))
    clauses ::= new Clause(List(-ind1, -(ind2+1), start+1))
    clauses ::= new Clause(List(-(start+1), ind1))
    clauses ::= new Clause(List(-(start+1), ind2+1))
    clauses ::= new Clause(List(-(ind1+1), -ind2, start+2))
    clauses ::= new Clause(List(-(start+2), ind1+1))
    clauses ::= new Clause(List(-(start+2), ind2))
    clauses ::= new Clause(List(-(ind1+1), -(ind2+1), start+3))
    clauses ::= new Clause(List(-(start+3), ind1+1))
    clauses ::= new Clause(List(-(start+3), ind2+1))

    clauses
  }


  // Simple 2 variable sanity test
  def twovarbntest(): Unit = {
    getManager(10)
    var clauses: List[Clause] = List[Clause]()
    // First are indicators for both variables
    // Indicators
    clauses ::= new Clause(List(1,2))
    clauses ::= new Clause(List(-1,-2))
    // clauses ::= new Clause(List(3,4))
    // clauses ::= new Clause(List(-3,-4))

    // Prior on first variable
    clauses ::= new Clause(List(1,-5))
    clauses ::= new Clause(List(-1,5))
    clauses ::= new Clause(List(2,-6))
    clauses ::= new Clause(List(-2,6))

    // Factor between the two
    // clauses ::= new Clause(List(-1, -3, 7))
    // clauses ::= new Clause(List(-7, 1))
    // clauses ::= new Clause(List(-7, 3))
    // clauses ::= new Clause(List(-1, -4, 8))
    // clauses ::= new Clause(List(-8, 1))
    // clauses ::= new Clause(List(-8, 4))
    // clauses ::= new Clause(List(-2, -3, 9))
    // clauses ::= new Clause(List(-9, 2))
    // clauses ::= new Clause(List(-9, 3))
    // clauses ::= new Clause(List(-2, -4, 10))
    // clauses ::= new Clause(List(-10, 2))
    // clauses ::= new Clause(List(-10, 4))

    // Set literal weights
    var lits: List[Tuple2[Long, Double]] = List[Tuple2[Long, Double]]()
    lits ::= ((5, 0.2))
    lits ::= ((6, 0.8))
    lits ::= ((7, 0.9))
    lits ::= ((8, 0.1))
    lits ::= ((9, 0.1))
    lits ::= ((10, 0.9))
    val sdd = compile(new CNF(clauses))
    var wmcman = new WmcManager(sdd, false)
    lits.map(x ⇒ wmcman.setLiteralWeight(x._1, x._2))
    wmcman.propagate()
    println(wmcman.getProbability(1))
  }

  def entailmentTest(): Unit = {
    getManager(3)
    var clauses: List[Clause] = List[Clause]()
    // A v B ⇒ C
    clauses ::= new Clause(List(-1, -3))
    clauses ::= new Clause(List(-2, -3))

    val sdd = Compiler.compile(new CNF(clauses))
    val wmcman = new WmcManager(sdd, false)
    println(wmcman.propagate())
    println(wmcman.getDerivative(3))

    wmcman.setLiteralWeight(-1, wmcman.getZeroWeight)
    println(wmcman.propagate())
    println(wmcman.getDerivative(-3))
  }
  // This is built on the example in Figure 11.4 of "Modeling and and reasoning with Bayesian Networks" by Darwiche
  def multiValCompileTest(): Unit = {
    getManager(13)
    var clauses: List[Clause] = List[Clause]()
    // First 5 are indicator variables: Ia1, Ia2, Ia3, Ib1, Ib2
    clauses ::= new Clause(List(1,2,3))
    clauses ::= new Clause(List(-1,-2))
    clauses ::= new Clause(List(-1,-3))
    clauses ::= new Clause(List(-2,-3))
    clauses ::= new Clause(List(4,5))
    clauses ::= new Clause(List(-4,-5))
    // Next 2 are Pb1, Pb2
    clauses ::= new Clause(List(-4, 6))
    clauses ::= new Clause(List(4, -6))
    clauses ::= new Clause(List(5, -7))
    clauses ::= new Clause(List(-5, 7))
    // Last 6 are Pa1|b1, Pa1|b2, Pa2|b1,...
    // clauses for Pa1|b1
    clauses ::= new Clause(List(-1,-4,8))
    clauses ::= new Clause(List(-8,1))
    clauses ::= new Clause(List(-8,4))
    // clauses for Pa1|b2
    clauses ::= new Clause(List(-1,-5,9))
    clauses ::= new Clause(List(-9,1))
    clauses ::= new Clause(List(-9,5))
    // clauses for Pa2|b1
    clauses ::= new Clause(List(-2,-4,10))
    clauses ::= new Clause(List(-10,2))
    clauses ::= new Clause(List(-10,4))
    // clauses for Pa2|b2
    clauses ::= new Clause(List(-2,-5,11))
    clauses ::= new Clause(List(-11,2))
    clauses ::= new Clause(List(-11,5))
    // clauses for Pa3|b1
    clauses ::= new Clause(List(-3,-4,12))
    clauses ::= new Clause(List(-12,3))
    clauses ::= new Clause(List(-12,4))
    // clauses for Pa3|b2
    clauses ::= new Clause(List(-3,-5,13))
    clauses ::= new Clause(List(-13,3))
    clauses ::= new Clause(List(-13,5))
    val sdd: Sdd = compile(new CNF(clauses))
    val wmcman: WmcManager = new WmcManager(sdd, false)
    wmcman.setLiteralWeight(6, 3.0)
    wmcman.setLiteralWeight(7, 5.0)
    wmcman.setLiteralWeight(8, 3.6)
    wmcman.setLiteralWeight(9, 7.0)
    wmcman.setLiteralWeight(10, 4.5)
    wmcman.setLiteralWeight(11, 17.0)
    wmcman.setLiteralWeight(12, 4.0)
    wmcman.setLiteralWeight(13, 3.5)
    println(wmcman.propagate())
    println(wmcman.getProbability(1))
    println(wmcman.getProbability(2))
    println(wmcman.getProbability(3))

  }
}

