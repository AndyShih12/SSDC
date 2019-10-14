import java.io._
import scala.io.Source
import scala.collection.mutable.ListBuffer

/**
  */
class Clause(val lits: List[Int]) {
  def disjoin(other: Clause): Clause = {
    new Clause(lits ::: other.lits)
  }

  def max: Int = {
    lits.map(x ⇒ Math.abs(x)).maxBy(y ⇒ y)
  }
}

class CNF(val clauses: List[Clause]) {
  def conjoin(other: CNF): CNF = {
    new CNF(clauses ::: other.clauses)
  }

  def conjoin(other: Clause): CNF = {
    new CNF(other :: clauses)
  }

  def dump(filename: String) = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    var towrite = List(Node.getSDDVarCount, clauses.length)
    bw.write("p cnf %d %d".format(Node.getSDDVarCount, clauses.length) + "\n")
    clauses.map(x ⇒ bw.write(x.lits.map(_.toString).mkString(" ") + " 0\n"))
    bw.close()
  }

  def max: Int = {
    clauses.map(_.max).maxBy(y ⇒ y)
  }
}

object CNF {
  // Read a CNF from a file
  def fromFile(file: String): CNF = {
    val lines = Source.fromFile(file).getLines.toList
    val clauses: ListBuffer[Clause] = ListBuffer[Clause]()
    for(line ← lines) {
      val tokens = line.split("\\s+").toList
      // Filter through comments and info crap
      if(!(tokens(0).equals("p")) && !(tokens(0).equals("c"))) {
        val lits = tokens.map(_.toInt)
        assume(lits(lits.length-1) == 0)
        clauses += new Clause(lits.slice(0, lits.length-1))
      }
    }
    new CNF(clauses.toList)
  }
  // Creates a bijection from the conjunction of a and b
  def bijection(a: List[Int], b: Int): CNF = {
    var ab = new Clause(b :: a.map(x ⇒ -x))
    new CNF(ab :: a.map(x ⇒ new Clause(List(x, -b))))
  }

  // Conjoin a list of CNFs into one CNF
  def conjoin(a: Iterable[CNF]): CNF = {
    a.fold(emptyCNF)(_.conjoin(_))
  }

  // Return empty CNF
  def emptyCNF(): CNF = {
    new CNF(List())
  }
}


