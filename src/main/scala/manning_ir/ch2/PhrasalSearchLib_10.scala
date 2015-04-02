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
		leftInd: Long, // left index
		rightInd: Long, // right index
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
  		.getLines.flatMap(preprocess(_)) // bypass lines for direct mapping of tokens
  		.foldLeft(tokenLocation){ // incoming location of immediate prededecessor
  			(tokenLocation, token) => { // previous token's location, current token
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
						term.termCount += 1 // TODO: Would it be better to tally all at once at the end? Probably not, because that would require another n worth of operations
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
	  			
				tokLocIncr // return token location (doc index) increment to foldLeft
				
	  		} // end (tokenLocation, token)
  		} // end Source.fromFile foldleft
	} // end indexDocument
		
	// Returns query results by frequency rank
	def freqRank(rankRounding: Int = 5)(query: String): QueryTermResults = {
		val qTerms = preprocess(query) // tokenize and lowercase query
		// Disjunction of query terms, initial step
		val termsRetrieved = qTerms.map(dictionary.get(_)).flatten // flatten filters out None
		val rankedIncidences = termsRetrieved.map( termIn => { // iterate each Term
			val termIncidences = termIn.allIncidences.values.map(docLocations => { // iterate each document's set of locations for this term
				val termWordCount = docLocations.termCount.toDouble // total count of term in this document
				val docWordCount: Option[Long] = { // number of words in this document
					docCatalog.get(docLocations.docId) match {
						case Some(doc) => Some(doc.tokensTotal)
						case None => Some(1) // TODO: This is vulnerable, None would cause exception at current tf calculation. Needs some re-work; in the meantime changing to 1
					}
				}
				val tf = roundAt(rankRounding)(termWordCount / docWordCount.get.toDouble)
				(docLocations.docId, Incidences(docLocations, tf))
			})
			termIncidences.toMap // TODO: This causes another n length cycle. Expensive at large scale
		}) // end termsRetrieved.map
		rankedIncidences.toList
	} // end freqRank
	
	// TODO: Expecting a dependancy on preserved order of query in results, but not implemented fully
	// TODO: What about biword ranking?
	// TODO: Assemble tuples of as much of the phrase as is found
	// QueryTermResults = List[scala.collection.immutable.Map[Long, Incidences]]
	//                  = ListOfAllTerms[OneTerm'sMapOfAllDocs[docId, OneDocIncidences]]
	def biWord(queryTermResults: QueryTermResults): List[List[BiWord]] = {
		// Recursive
		def inter(terms: QueryTermResults, accum: List[List[BiWord]]): List[List[BiWord]] = {
			terms match {
				case leftTerm :: Nil => accum // we're done!
				case leftTerm :: xs => { // leftTerm is a Map of Instances per docId for this term
					val biwords = for{
			  			leftDocIncidences <- leftTerm.values // iterate each individual document's Incidences for this term
						leftTermIndexInDoc <- leftDocIncidences.locs.indexes // iterate each location of the left term in this doc
	   					leftTermStr = leftDocIncidences.locs.term // term (string)
	   					docId = leftDocIncidences.locs.docId // the id of the doc being queried
	   					// Now see if the next term in the phrase query is also in this document, 
	   					// and if so, is the location value leftIndex + 1
	   					rightTermLocationIndex = xs.head.get(docId) match { // right term locations in this document
	   						case Some(incds) => { // Yes, this document also contains the next term in the phrase
	   							// So does that term exist at leftIndex + 1 ?
	   							incds.locs.indexes.toList.indexOf(leftTermIndexInDoc + 1) // TODO: This should be refactored to return the list of indexes if they exist so as to not repeat the operation in the yield below
	   						}
	   						case None => -1 // next term not in this doc
	   					}  						
	  					if rightTermLocationIndex > -1 // if true, we've found a sequential pair
			  		} yield { // therefor
			  			// TODO: This series of dots is ugly
			  			// This is not actually necessary, since we already know the term exists and its index, however, this proves it, which is good while in development
			  			val rightTermIndexInDoc = xs.head.get(docId).get.locs.indexes.toList(rightTermLocationIndex)
			  			val rightTermStr = xs.head.get(docId).get.locs.term
			  			// package up the sequential pair of phrase terms we've found in our document
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




