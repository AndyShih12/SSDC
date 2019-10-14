/**
  */
import scala.collection.mutable.ListBuffer

trait Node {
  def getCNF: CNF
}

class FactorNode(val varNodes: List[VarNode], val contents: Map[List[Int], Double]=null) extends Node {
  // Every entry in the map should already be present, so let's just use that
  var sddIDs: Map[List[Int], Int] = null
  if(contents != null) {
    sddIDs = (contents.keys.toList zip contents.values.map(_ => Node.nextSDDVar())).toMap
  }

  def getCNF(): CNF = {
    // This shouldn't ever get called if we don't know about the actual factors
    assert(contents != null)
    CNF.conjoin(contents.keys.map(x ⇒ CNF.bijection((x zip varNodes).map({case (i, v) ⇒ v.sddIDs(i)}), sddIDs(x))))
  }

  // For sampling purposes, define a size for this factor
  def size: Int = {
    varNodes.length
  }

  override def toString: String = {
    contents.toString
  }

}

class VarNode(var n: Int) extends Node {
  var sddIDs: ListBuffer[Int] = (for(i ← List.range(0, n)) yield Node.nextSDDVar).to[ListBuffer]
  def getCNF(): CNF = {
    var clauses: List[Clause] = List[Clause](new Clause(sddIDs.toList))
    for(i ← 1 to n-1) {
      for(j ← 0 to i-1) {
        clauses ::= new Clause(List(-sddIDs(i), -sddIDs(j)))
      }
    }
    new CNF(clauses)
  }
}

object Node {
  var curr = 0
  def nextSDDVar(): Int = {
    curr += 1
    return curr
  }
  def getSDDVarCount(): Int = {
    return curr
  }
}


