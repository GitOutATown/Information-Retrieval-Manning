package manning_ir.ch2

import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.SortedSet

object PhrasalSearch_lab_1 {

	// Document specific data
	case class Document(
		val id: Long,
		val name: String,
		val author: String,
		var tokensTotal: Long = 0,
		var tokensUniqueSize: Option[Long] = None,
		var tokensUnique: SortedSet[String] = SortedSet.empty[String]
	)
	
	// Count and locations of all instances of a term (type) within a document
	case class TermIncidences(
		val docId: Long,
		var count: Int, // number of instances of term in document
		var locs: List[Int]
	)
	// sort TermIncidences by docId
	object TermIncidencesOrdering extends Ordering[TermIncidences] {
	  def compare(a:TermIncidences, b:TermIncidences) = a.docId compare b.docId
	}
	
	// Term specific data within a collection or corpus
	case class Term(
		val term: String,
		var termFrequency: Long,
		val postings: Map[Long, TermIncidences] // key is docId
	)
	
	// Term lookup
	val dictionary = Map.empty[String, Term]  //> dictionary  : scala.collection.mutable.Map[String,manning_ir.ch2.PhrasalSea
                                                  //| rch_lab_1.Term] = Map()
	
	// Document catalog
	val docCatalog = Map.empty[Long, Document]//> docCatalog  : scala.collection.mutable.Map[Long,manning_ir.ch2.PhrasalSearc
                                                  //| h_lab_1.Document] = Map()
	
  	// Document path
	val root_path = "/Users/hieronymus/Development/Workspace_BAK/Intro IR Manning/"
                                                  //> root_path  : String = /Users/hieronymus/Development/Workspace_BAK/Intro IR 
                                                  //| Manning/
  val doc_path = "src/main/resources/shakespeare/"//> doc_path  : String = src/main/resources/shakespeare/
  
  	// Inverse index model: dictionary of term postings.
  	// TermIncidences are count and location of all occurances of a term within a document.
  def indexDocument(doc: Document) {
  		val lineNum = new java.util.concurrent.atomic.AtomicLong(1L)
  		def nextlineNum = lineNum.getAndIncrement
  		
  		for (line <- Source.fromFile(root_path + doc_path + doc.name).getLines) {
  			val lineTokens = line.split("[ !,.:;]+") //("\\s+") // split on any number of contiguous space characters
			//doc.tokensTotal += lineTokens.length
			
			lineTokens.map(tok => dictionary.get(tok) match {
				case Some(term) => {
					
					doc.tokensUnique += tok
				} // end Some
				case None => {
					val postings = SortedSet.empty(TermIncidencesOrdering)
					//val incidences = TermIncidences(doc.id, 1, )
					//val term = Term(tok, 0, postings)
				} // end None
			}) // end tokens.map
  		} // end for line
  } // end indexDocument                          //> indexDocument: (doc: manning_ir.ch2.PhrasalSearch_lab_1.Document)Unit
  
  // ----------------------------------- //
  
  //Term("huh", 9999, SortedSet.empty(TermIncidencesOrdering))
  0                                               //> res0: Int(0) = 0
}
/*



*/