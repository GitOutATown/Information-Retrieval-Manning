package manning_ir.ch2

import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.SortedSet

/**
 * Exercise in phrasal search. Not using POS or stop words.
 * Includes Ranking.
 * Some open questions:
 * * Whether to use a sliding window strategy (on the query
 * * Basis for ranking
 * * Whether to use a proximity parameter on biword pairs (affects ranking)
 * * How to account for repeat terms in query
 */
object PhrasalSearch_lab_2 {

	// Document specific data
	case class Document(
		val id: Long,
		val name: String,
		val author: String,
		var tokensTotal: Long = 0,
		var tokensUnique: SortedSet[String] = SortedSet.empty[String]
	)
	
	// Count and locations of all instances of a term (type) within a document
	case class Incidences(
		val docId: Long,
		var count: Long, // number of instances of term in document
		val locs: SortedSet[Long] // location of each instance of the term in this document
	)
	
	// Term specific data within a collection or corpus
	case class Term(
		val term: String, // unique token (type, word)
		var termFrequency: Long, // total number of times this term appears in the corpus
		val incidences: Map[Long, Incidences] // key is docId, Incidences are the locations of the term per each document
	)
	
	// Term lookup. Mutable (side effect)
	val dictionary = Map.empty[String, Term]  //> dictionary  : scala.collection.mutable.Map[String,manning_ir.ch2.PhrasalSea
                                                  //| rch_lab_2.Term] = Map()
	
	// Document catalog. Mutable (side effect)
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
  		.foldLeft(tokenLocation){ // location of immediate prededecessor
	  		(tokenLocation, token) => {
	  			val tokLocIncr = tokenLocation + 1 // increment token location
	  			doc.tokensTotal += 1
	  			// record location of this instance
	  			dictionary.get(token) match {
					case Some(term) => { // retrieving term from dictionary
						val incidences = term.incidences.get(doc.id) match {
							case Some(incidences) => {
								incidences.locs += tokLocIncr
								incidences
							}
							case None => Incidences(doc.id, 1, SortedSet(tokLocIncr)) // 1 is the first count
						}
						term.termFrequency += 1 // TODO: I don't like this constant mutation. Better to tally all at once at the end.
						doc.tokensUnique += token // This doesn't seem necessary, but what the hell...
					}
					case None => { // term does not exist in dictionary, create it
						val incidences = Incidences(doc.id, 1, SortedSet(tokLocIncr)) // 1 is the first count
						val term = Term(token, 1, Map[Long, Incidences](doc.id -> incidences))
						dictionary += (token -> term)
						doc.tokensUnique += token
					}
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
	  Document(IDGenerator.next, "Antony_and_Cleopatra.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Hamlet.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Julius_Caesar.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Macbeth.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Othello.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "The_Tempest.txt", "William Shakespear")
  )                                               //> docList  : List[manning_ir.ch2.PhrasalSearch_lab_2.Document] = List(Documen
                                                  //| t(100,Antony_and_Cleopatra.txt,William Shakespear,0,TreeSet()), Document(10
                                                  //| 1,Hamlet.txt,William Shakespear,0,TreeSet()), Document(102,Julius_Caesar.tx
                                                  //| t,William Shakespear,0,TreeSet()), Document(103,Macbeth.txt,William Shakesp
                                                  //| ear,0,TreeSet()), Document(104,Othello.txt,William Shakespear,0,TreeSet()),
                                                  //|  Document(105,The_Tempest.txt,William Shakespear,0,TreeSet()))
  
  // Put docs in Catalog (i.e. Map)
  docList.foreach(doc => docCatalog += (doc.id -> doc))
  
  // ------ Indexing ------------------- //
  
  // Initiate indexing
  docList foreach indexDocument
  
  
    
  	// ------ Logging ndexing results ------ //
	
	dictionary.keys.size                      //> res0: Int = 13743
	
	docList foreach(doc => {
		println("------------------")
		println(doc.name)
		println("Unique terms: " + doc.tokensUnique.size)
		println("Word count: " + doc.tokensTotal)
	})                                        //> ------------------
                                                  //| Antony_and_Cleopatra.txt
                                                  //| Unique terms: 4775
                                                  //| Word count: 27137
                                                  //| ------------------
                                                  //| Hamlet.txt
                                                  //| Unique terms: 5658
                                                  //| Word count: 32320
                                                  //| ------------------
                                                  //| Julius_Caesar.txt
                                                  //| Unique terms: 3551
                                                  //| Word count: 20928
                                                  //| ------------------
                                                  //| Macbeth.txt
                                                  //| Unique terms: 4031
                                                  //| Word count: 18314
                                                  //| ------------------
                                                  //| Othello.txt
                                                  //| Unique terms: 4613
                                                  //| Word count: 28026
                                                  //| ------------------
                                                  //| The_Tempest.txt
                                                  //| Unique terms: 3809
                                                  //| Word count: 17468
	
}
/*








*/