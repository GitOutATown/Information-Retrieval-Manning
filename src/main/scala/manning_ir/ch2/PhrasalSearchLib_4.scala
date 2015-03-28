package manning_ir.ch2

import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.SortedSet

object PhrasalSearchLib_4 {

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
	val dictionary = Map.empty[String, Term]
	
	// Document catalog. Mutable (side effect)
	val docCatalog = Map.empty[Long, Document]
 
  	// Inverse index model: dictionary of term postings.
	def indexDocument(path: String)(doc: Document) {
  		val tokenLocation = 0 // token location initialization for document
  		Source.fromFile(path + doc.name)
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
	} // end indexDocument
}




