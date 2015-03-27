package manning_ir.ch2

import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.SortedSet

object PhrasalSearch_lab_1 {

	case class Document(
		val id: Long,
		val name: String,
		val author: String,
		var tokensTotal: Long = 0,
		var tokensUniqueSize: Option[Long] = None,
		var tokensUnique: SortedSet[String] = SortedSet.empty[String]
	)
	
	// Count and locations of all instances of a term within a document
	case class Postings(
		docId: Long,
		count: Int, // number of instances of term in document
		locs: List[Int]
	)
	// sort by docId
	object PostingsOrdering extends Ordering[Postings] {
	  def compare(a:Postings, b:Postings) = a.docId compare b.docId
	}
	
	case class Term(
		term: String,
		termFrequency: Long,
		postings: SortedSet[Postings]
	)
	
	val dictionary = Map.empty[String, Term]  //> dictionary  : scala.collection.mutable.Map[String,manning_ir.ch2.PhrasalSear
                                                  //| ch_lab_1.Term] = Map()
  // ----------------------------------- //
  
  Term("huh", 9999, SortedSet.empty(PostingsOrdering))
                                                  //> res0: manning_ir.ch2.PhrasalSearch_lab_1.Term = Term(huh,9999,TreeSet())
}
/*



*/