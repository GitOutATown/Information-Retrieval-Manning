package manning_ir.ch2

import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.SortedSet

import util.math.Rounding_lab_1._

object PhrasalSearchLib_10 {

	// Document specific data
	case class Document(
		val id: Long,
		val name: String,
		val author: String,
		var tokensTotal: Long = 0,
		var tokensUnique: SortedSet[String] = SortedSet.empty[String]
	)
	
	// Count and locations of all instances of a term (type) within a document
	case class Locations(
		val docId: Long,
		val term: String,
		var termCount: Long, // number of instances of term in document
		val indexes: SortedSet[Int] // location of each instance of the term in this document
	)
	
	// Term specific data within a collection or corpus
	case class Term(
		val term: String, // unique token (type, word)
		var termCount: Long, // total number of times this term appears in the corpus
		val allIncidences: Map[Long, Locations] // [docId, All incidences of term in multiple docs]
	)
	
	// All incidences of a single term within one document
	// Used for query results
	case class Incidences(
		val locs: Locations,
		val rankMetric: Double // document frequency
	)
	object IncidencesOrdering extends Ordering[Incidences] {
		def compare(a: Incidences, b: Incidences) = a.rankMetric compare b.rankMetric
	}
	
	case class BiWord(
	    docId: Long,
		leftInd: Long,
		rightInd: Long,
		leftTerm: String,
		rightTerm: String
	)
	
	object IDGenerator {
	  private val n = new java.util.concurrent.atomic.AtomicLong(100L)
	  def next = n.getAndIncrement
	}
	
	type QueryTermResults = List[scala.collection.immutable.Map[Long, Incidences]]
	
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
	  			val tokLocIncr = tokenLocation + 1 // increment token location
	  			doc.tokensTotal += 1
	  			
	  			// record location of this instance, uses side effects
	  			dictionary.get(token) match {
					case Some(term) => { // retrieving term from dictionary
						val docLocations = term.allIncidences.get(doc.id) match {
							case Some(docIncidences) => {
								docIncidences.indexes += tokLocIncr
								docIncidences.termCount += 1
								docIncidences
							}
							case None => Locations(doc.id, token, 1, SortedSet(tokLocIncr)) // 1 is the first count
						}
						term.termCount += 1 // TODO: Would it be better to tally all at once at the end?
						term.allIncidences += (doc.id -> docLocations)
						doc.tokensUnique += token // This doesn't seem necessary, but what the hell...
					}
					case None => { // term does not exist in dictionary, create it
						val docIncidences = Locations(doc.id, token, 1, SortedSet(tokLocIncr)) // 1 is the first count
						val term = Term(token, 1, Map[Long, Locations](doc.id -> docIncidences))
						dictionary += (token -> term)
						doc.tokensUnique += token
					}
				} // end term match
	  			
				tokLocIncr // return token location increment to foldLeft
				
	  		} // end (tokenLocation, token) =>
  		} // end Source.fromFile foldleft
	} // end indexDocument
		
	// Returns query results by frequency rank
	def freqRank(rankRounding: Int = 5)(query: String)
	:List[scala.collection.immutable.Map[Long, Incidences]] = {
		// tokenize and lowercase query
		val qTerms = preprocess(query)
		// Disjunction of query terms, initial step
		val termsRetrieved = qTerms.map(dictionary.get(_)).flatten
		val rankedIncidences = termsRetrieved.map( termIn => { // iterate over each Term
			val termIncidences = termIn.allIncidences.values.map(docLocations => { // iterate through all document locations of this term
				val termWordCount = docLocations.termCount.toDouble // total count of term in this document
				val docWordCount: Option[Long] = { // number of words in this document
					docCatalog.get(docLocations.docId) match {
						case Some(doc) => Some(doc.tokensTotal)
						case None => None
					}
				}
				val tf = roundAt(rankRounding)(termWordCount / docWordCount.get.toDouble)
				(docLocations.docId, Incidences(docLocations, tf))
			})
			termIncidences.toMap
		}) // end termsRetrieved.map
		rankedIncidences.toList
	} // end freqRank
	
	// Dependancy on preserved order of query in results
	// TODO: What about biword ranking?
	// What about assembling tuples of as much of the phrase as is found?
	// QueryTermResults = List[scala.collection.immutable.Map[Long, Incidences]]
	//                  = ListOfAllTerms[OneTerm'sMapOfAllDocs[docId, OneDocIncidences]]
	def biWord(queryTermResults: QueryTermResults): List[List[BiWord]] = {
		// Recursive
		def inter(terms: QueryTermResults, accum: List[List[BiWord]]): List[List[BiWord]] = {
			terms match {
				case leftTerm :: Nil => accum
				case leftTerm :: xs => { // leftTerm is a Map of Instances per docId for this term
					val biwords = for{
			  			leftDocIncidences <- leftTerm.values // iterate each individual document's Incidences for this term
						leftTermIndexInDoc <- leftDocIncidences.locs.indexes // iterate each location of the left term in this doc
	   					leftTermStr = leftDocIncidences.locs.term  //leftDocQueryResult.docIncidences.term
	   					docId = leftDocIncidences.locs.docId // the id of the doc being queried
	   					// Now to see if the next term has this document, and if so,
	   					// does the document have a location value of leftIndex + 1
	   					rightTermLocationIndex:Int = xs.head.get(docId) match { // right term locations in this document
	   						case Some(incds) => { // Yes, this document also contains the next term in the phrase
	   							// So does that term exist at leftIndex + 1 ?
	   							incds.locs.indexes.toList.indexOf(leftTermIndexInDoc + 1)
	   						}
	   						case None => -1
	   					}  						
	  					if rightTermLocationIndex > -1 // if true, we found a sequential pair
			  		} yield {
			  			// TODO: This series of dots is ugly
			  			val rightTermIndexInDoc = xs.head.get(docId).get.locs.indexes.toList(rightTermLocationIndex)
			  			val rightTermStr = xs.head.get(docId).get.locs.term
			  			BiWord(
			  				docId,
			  				leftTermIndexInDoc, rightTermIndexInDoc,
			  				leftTermStr, rightTermStr
			  			)
		  			}
			  		inter(xs, accum ++ List(biwords.toList)) // recurse next term
				} // end case x :: xs
				
			} // end terms match
		} // end inter
		inter(queryTermResults, List.empty[List[BiWord]])
	} // end biWord
}




