package manning_ir.ch2

import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.SortedSet

import util.math.Rounding_lab_1._

object PhrasalSearchLib_9 {

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
		val term: String,
		var termCount: Long, // number of instances of term in document
		val locs: SortedSet[Long] // location of each instance of the term in this document
	)
	
	// Term specific data within a collection or corpus
	case class Term(
		val term: String, // unique token (type, word)
		var termCount: Long, // total number of times this term appears in the corpus
		val incidences: Map[Long, Incidences]
	)
	
	// All incidences of a single term within one document
	case class QueryResult(
		val incidences: Incidences,
		val rankMetric: Double // document frequency
	)
	object QueryOrdering extends Ordering[QueryResult] {
		def compare(a: QueryResult, b: QueryResult) = a.rankMetric compare b.rankMetric
	}
	
	object IDGenerator {
	  private val n = new java.util.concurrent.atomic.AtomicLong(100L)
	  def next = n.getAndIncrement
	}
	
	// Term lookup. Mutable (side effect)
	val dictionary = Map.empty[String, Term]
	
	// Document catalog. Mutable (side effect)
	val docCatalog = Map.empty[Long, Document]
	
	def preprocess(str: String): Array[String] = {
	  str.split("[ !,.:;]+").map(_.toLowerCase)
	}
 
  	// Inverse index model: dictionary of term postings.
	def indexDocument(path: String)(doc: Document) {
  		val tokenLocation = 0 // token location initialization for document
  		
  		Source.fromFile(path + doc.name)
  		.getLines.flatMap(preprocess(_)) // bypass lines to direct mapping of tokens
  		.foldLeft(tokenLocation){ // location of immediate prededecessor
  			(tokenLocation, token) => {
	  			val tokLowCase = token.toLowerCase
	  			val tokLocIncr = tokenLocation + 1 // increment token location
	  			doc.tokensTotal += 1
	  			
	  			// record location of this instance, uses side effects
	  			dictionary.get(tokLowCase) match {
					case Some(term) => { // retrieving term from dictionary
						val incidences = term.incidences.get(doc.id) match {
							case Some(incidences) => {
								incidences.locs += tokLocIncr
								incidences.termCount += 1
								incidences
							}
							case None => Incidences(doc.id, tokLowCase, 1, SortedSet(tokLocIncr)) // 1 is the first count
						}
						term.termCount += 1 // TODO: Would it be better to tally all at once at the end?
						term.incidences += (doc.id -> incidences)
						doc.tokensUnique += tokLowCase // This doesn't seem necessary, but what the hell...
					}
					case None => { // term does not exist in dictionary, create it
						val incidences = Incidences(doc.id, tokLowCase, 1, SortedSet(tokLocIncr)) // 1 is the first count
						val term = Term(tokLowCase, 1, Map[Long, Incidences](doc.id -> incidences))
						dictionary += (tokLowCase -> term)
						doc.tokensUnique += token
					}
				} // end term match
	  			
				tokLocIncr // return token location increment to foldLeft
				
	  		} // end (tokenLocation, token) =>
  		} // end Source.fromFile foldleft
	} // end indexDocument
	
	// Query results by frequency rank
	def freqRank(rankRounding: Int = 5)(query: String): List[List[QueryResult]] = {
		// tokenize and lowercase query
		val qTerms = preprocess(query)
		
		// Disjunction of query terms, initial step
		val termsRetrieved = qTerms.map(dictionary.get(_)).flatten
		
		// Sort each set of term results by ranking (tf).
		val sortedRankings = for{
			term <- termsRetrieved
		} yield {
			for{
				incidences <- term.incidences.values
				val termWordCount = incidences.termCount.toDouble
				val docWordCount: Option[Long] = {
					docCatalog.get(incidences.docId) match {
						case Some(doc) => Some(doc.tokensTotal)
						case None => None
					}
				}
				if docWordCount.isDefined
			} yield {
				val tf = roundAt(rankRounding)(termWordCount / docWordCount.get.toDouble)
				QueryResult(incidences, tf)
			}
		}.toList.sorted(QueryOrdering)
		
		sortedRankings.toList
		
	} // end phrasalQuery
}




