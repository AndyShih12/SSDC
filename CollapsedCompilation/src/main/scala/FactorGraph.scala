/**
  */
import sdd.Sdd
import scala.io.Source
import java.io.File
import scala.collection.mutable.ListBuffer

trait FactorGraph {
  // factorAssignments represents assignments in the actual factor graph, assignments is for SDD variables
  var factorAssignments: Map[VarNode, Int] = Map[VarNode, Int]()
  var assignments: Map[Long, Boolean] = Map[Long, Boolean]()
  val vars: List[VarNode]
  var factors: List[FactorNode]
  // Generate empty evidence
  var evidence:Map[VarNode, Int] = Map[VarNode, Int]()

  def assignFactor(x: VarNode, ass: Int): Unit = {
    // Assign the factor graph node
    factorAssignments = factorAssignments + (x -> ass)
    // This has n corresponding indicator variables
    for(i ← 0 to x.n-1) {
      assignments = assignments + (x.sddIDs(i).toLong → (i == ass))
    }
  }

  def clearAssignments = {
    // Reset all assignments
    factorAssignments = Map[VarNode, Int]()
    assignments = Map[Long, Boolean]()
  }

  // Make sure that we only ever compute this once
  private var varmap: Map[VarNode, ListBuffer[FactorNode]] = null
  def getVarMap: Map[VarNode, ListBuffer[FactorNode]] = {
    if(varmap == null) {
      varmap = (for (v ← vars) yield v → ListBuffer[FactorNode]()).toMap
      factors.map(f ⇒ f.varNodes.map(v ⇒ varmap(v) += f))
    }
    varmap
  }

  // Should have a way to get a mapping from factors to SDD per factors
  def getFacSDDs: Map[FactorNode, Sdd]

  // Get a CNF representing the evidence
  def getEvidCNF(): CNF = {
    new CNF(evidence.map { case (v, a) => new Clause(List(v.sddIDs(a)))}.toList)
  }
}

class UFFactorGraph(var factors: List[FactorNode], val vars: List[VarNode]) extends FactorGraph {
  def getCNF: CNF = {
    CNF.conjoin((factors ::: vars).map(x ⇒ x.getCNF))
  }

  override def getFacSDDs: Map[FactorNode, Sdd] = {
    (this.factors zip this.factors.map(f ⇒ Compiler.compile(f.getCNF))).toMap
  }
}

class CFFactorGraph(val vars: List[VarNode], val facLits: List[Int], val facWeights: List[Double], var factors: List[FactorNode], sddDir: String) extends FactorGraph{
  // Assume the sdd manager has already been initialized, going to just read in all the SDDs
  val files = getListOfFiles(sddDir)
  val facSDDs = files.sortBy(x ⇒ getSddFileInd(x)).map(x ⇒ Sdd.read(x.getAbsolutePath, Compiler.getManager(0)))
  facSDDs.map(_.ref)

  
  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  override def getFacSDDs: Map[FactorNode, Sdd] = {
    // println(factors)
    // println(facSDDs)
    (factors zip facSDDs).toMap
  }

  private def getSddFileInd(file: File): Int = {
    val fstring = file.getAbsolutePath
    val tokens = fstring.split('/')
    tokens(tokens.length - 1).split("sdd")(0).toInt
  }

  // Read in a uai evidence file and update evidence map accordingly
  def evidenceFromFile(file: String): Unit = {
    // Load in the one line of the file
    val line = Source.fromFile(file).getLines.toList.mkString(" ")
    val entries: Array[Int] = line.split("\\s+").filterNot(_ == "").map(x ⇒ x.toInt)
    for(i ← 1 to entries(0)) {
      println(entries(2*i-1))
      println(entries(2*i))
      println(vars(entries(2*i-1)))
      evidence += (vars(entries(2*i-1)) → entries(2*i))
    }
  }
}



object FactorGraph {
  // Build a CF (CompiledFactors) FactorGraph from a litmaps file, directory of SDDs, and uai file
  def fromLitmap(file: String, sddDir: String, uaifile: String): CFFactorGraph = {
    // Load in the file
    val lines = Source.fromFile(file).getLines.toList
    var varNodes: ListBuffer[VarNode] = new ListBuffer[VarNode]()
    var facLits: ListBuffer[Int] = new ListBuffer[Int]()
    var facWeights: ListBuffer[Double] = new ListBuffer[Double]()
    for(line ← lines) {
      require(line != null)
      if(line.startsWith("cc$")) {
        // Tokenize
        val tokens = line.trim.split("\\$")
        val lineType = tokens(1)
        if(lineType.equals("V")) {
          val varnum = tokens(2).filter(_.isDigit).toInt
          val arity = tokens(3).toInt
          // Make sure it's going in order as we assume
          assume(varnum == varNodes.length)
          varNodes += new VarNode(arity)
        } else if(lineType.equals("I")) {
          // Finding all the indicators
          val litnum = tokens(2).toInt
          val varnum = tokens(5).filter(_.isDigit).toInt
          val varval = tokens(6).toInt
          varNodes(varnum).sddIDs(varval) = litnum
        } else if(lineType.equals("A")) {
          // We're going to ignore negative literals, since they never tell us anything
          if(tokens(2).toInt > 0) {
            // Grabbing our list of literals and weights
            val lit = tokens(2).toInt
            val weight = tokens(3).toDouble
            facLits += lit
            facWeights += weight
          }
        }
      }
    }
    // Load in uai file so we can just build the graph itself
    val ulines = Source.fromFile(uaifile).getLines.toList
    val numFacs: Int = ulines(3).toInt
    var facNodes: Array[FactorNode] = new Array[FactorNode](numFacs)
    for(i ← 0 to numFacs - 1) {
      val labels: Array[Int] = ulines(i+4).split("\\s+").filterNot(_ == "").map(x => x.toInt)
      val varList: Array[VarNode] = labels.slice(1, labels.length).map(x => varNodes(x)).reverse
      facNodes(i) = new FactorNode(varList.toList)

    }
        new CFFactorGraph(varNodes.toList, facLits.toList, facWeights.toList, facNodes.toList, sddDir)
  }
  // Build a UF(UncompiledFactors) FactorGraph from a.uai file
  def fromFile(file: String): UFFactorGraph = {
    // Load in the file
    val lines = Source.fromFile(file).getLines.toList
    // Figure out how many variables and instantiate
    val numVars: Int = lines(1).toInt
    val cards: Array[Int] = lines(2).split("\\s+").filterNot(_ == "").map(x ⇒ x.toInt)
    var varNodes: Array[VarNode] = new Array[VarNode](numVars)
    for(i <- 0 to numVars-1) {
      varNodes(i) = new VarNode(cards(i))
    }
    // Figure out how many factors and grab them
    val numFacs: Int = lines(3).toInt
    var facNodes: Array[FactorNode] = new Array[FactorNode](numFacs)
    for(i <- 0 to numFacs-1) {
      // There's some pretty messy indexing here, blame the UAI format
      val labels: Array[Int] = lines(i+4).split("\\s+").filterNot(_ == "").map(x => x.toInt)
      val numEntries: Int = lines(4+numFacs+2*i).toInt
      val entries: Array[Double] = lines(4+numFacs+2*i+1).split("\\s+").filterNot(_ == "").map(x => x.toDouble)
      assert(numEntries == entries.length)
      assert(labels.length == labels(0) + 1)
      val varList: Array[VarNode] = labels.slice(1, labels.length).map(x => varNodes(x)).reverse
      var facMap: Map[List[Int], Double] = Map[List[Int], Double]()
      // Keep track of which state we're taking in for each variable, this is parallel to varList
      val currInds: Array[Int] = for(i ← (1 to varList.length).toArray) yield 0
      for(j <- 0 to numEntries - 1) {
        facMap += (currInds.toList -> entries(j))
        // Incrememnt in first variable, propagate through
        currInds(0) += 1
        for(i ← 0 to varList.length - 2) {
          if(currInds(i) >= varList(i).n) {
            currInds(i) -= varList(i).n
            currInds(i+1) += 1
          }
        }
      }
      facNodes(i) = new FactorNode(varList.toList, facMap)
    }

    return new UFFactorGraph(facNodes.toList, varNodes.toList)
  }
}
