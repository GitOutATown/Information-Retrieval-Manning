package manning_ir.ch2

import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.SortedSet

object PhrasalSearch_lab_2 {

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
		val locs: SortedSet[Int] // location of each instance of the term in this document
	)
	
	// Term specific data within a collection or corpus
	case class Postings(
		val term: String, // unique token (type, word)
		var termFrequency: Long, // total number of times this term appears in the corpus
		val incidences: Map[Long, TermIncidences] // key is docId, TermIncidences are the locations of the term per each document
	)
	
	// Postings lookup
	val dictionary = Map.empty[String, Postings]
                                                  //> dictionary  : scala.collection.mutable.Map[String,manning_ir.ch2.PhrasalSea
                                                  //| rch_lab_2.Postings] = Map()
	
	// Document catalog
	val docCatalog = Map.empty[Long, Document]//> docCatalog  : scala.collection.mutable.Map[Long,manning_ir.ch2.PhrasalSearc
                                                  //| h_lab_2.Document] = Map()
	
  	// Document path
	val root_path = "/Users/hieronymus/Development/Workspace_BAK/Intro IR Manning/"
                                                  //> root_path  : String = /Users/hieronymus/Development/Workspace_BAK/Intro IR 
                                                  //| Manning/
  val doc_path = "src/main/resources/shakespeare/"//> doc_path  : String = src/main/resources/shakespeare/
  
  	// Inverse index model: dictionary of term postings.
  def indexDocument(doc: Document) {
  		val tokenLocation = 0 // token location initialization for document
  		Source.fromFile(root_path + doc_path + doc.name)
  		.getLines.flatMap(_.split("[ !,.:;]+")) // bypass lines to direct mapping of tokens
  		.foldLeft(tokenLocation){ // immediate prededecessor token location
	  		(tokenLocation, token) => {
	  			val tokLocIncr = tokenLocation + 1 // increment token location
	  			doc.tokensTotal += 1
	  			dictionary.get(token) match { // retrieving term from dictionary
					case Some(postings) => {
						postings.incidences.get(doc.id).get.locs += tokLocIncr // record location of this instance
						postings.termFrequency += 1 // TODO: I don't like this constant mutation. Better to tally all at once at the end.
						doc.tokensUnique += token // This doesn't seem necessary, but what the hell...
					} // end Some
					case None => { // term does not exist in dictionary, create it
						val incidences = TermIncidences(doc.id, 1, SortedSet(tokLocIncr))
						val postings = Postings(token, 1, Map[Long, TermIncidences](doc.id -> incidences))
						dictionary += (token -> postings)
						doc.tokensUnique += token
					} // end None
				} // end match
				tokLocIncr // return token location increment to foldLeft
	  		} // end (tokLoc, token) =>
	  } // end Source.fromFile foldleft
  } // end indexDocument                          //> indexDocument: (doc: manning_ir.ch2.PhrasalSearch_lab_2.Document)Unit
  
  // ------- Documents ---------------- //
  
  object IDGenerator {
	  private val n = new java.util.concurrent.atomic.AtomicLong(100L)
	  def next = n.getAndIncrement
	}
	
	val docList = List[Document](
	  Document(IDGenerator.next, "Antony_and_Cleopatra.txt", "William Shakespear")
  )                                               //> docList  : List[manning_ir.ch2.PhrasalSearch_lab_2.Document] = List(Documen
                                                  //| t(100,Antony_and_Cleopatra.txt,William Shakespear,0,None,TreeSet()))
  // Put docs in Catalog (i.e. Map)
  docList.foreach(doc => docCatalog += (doc.id -> doc))
  
  // ------ Indexing ------------------- //
  
  // Initiate indexing
  docList foreach indexDocument
    
  	// ------ Logging ndexing results ------ //
	
	dictionary.keys.size                      //> res0: Int = 4775
	
	docList foreach(doc => {
		println("------------------")
		println(doc.name)
		println("Unique terms: " + doc.tokensUnique.size)
		println("Word count: " + doc.tokensTotal)
	})                                        //> ------------------
                                                  //| Antony_and_Cleopatra.txt
                                                  //| Unique terms: 4775
                                                  //| Word count: 27137
}
/*








*/