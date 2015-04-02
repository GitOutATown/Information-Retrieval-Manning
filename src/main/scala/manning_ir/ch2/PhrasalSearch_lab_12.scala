package manning_ir.ch2

import manning_ir.ch2.PhrasalSearchLib_10._
import util.math.Rounding_lab_1._

import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.immutable.Map
import scala.collection.mutable.SortedSet
import scala.math.log10
 
/**
 * Exercise in phrasal search. Not using POS or stop words.
 * Doing lowercasing of index and query.
 * Using Term Frequency for ranking. Not using IDF because collection is too small.
 * Some open questions:
 * * Whether to use a sliding window strategy (on the phrase query)
 * * Basis for phrase retreival ranking
 * * Whether to use a proximity parameter on biword pairs (affects ranking)
 * * How to account for repeat terms in query
 */
object PhrasalSearch_lab_12 extends App {
  	// Document path
	val root_path = "/Users/hieronymus/Development/Workspace_BAK/Intro IR Manning/"
	val doc_path = "src/main/resources/shakespeare/"
	
	val docList = List[Document](
		Document(IDGenerator.next, "Antony_and_Cleopatra.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Hamlet.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Julius_Caesar.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Macbeth.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Othello.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "The_Tempest.txt", "William Shakespear")
	)
  
	// Put docs in Catalog (i.e. Map)
	docList.foreach(doc => docCatalog += (doc.id -> doc))
  
	// ------ Indexing ------------------- //
  
	// partial setup with doc paths
	val index = indexDocument(root_path + doc_path)_
  
	// Initiate indexing
	docList foreach index
 
  	// ------ Logging indexing results ------ //
	
	dictionary.keys.size
	
	docList foreach(doc => {
		println("------------------")
		println(doc.name)
		println("Unique terms: " + doc.tokensUnique.size)
		println("Word count: " + doc.tokensTotal)
	})
  
	// ----- Phrasal Querying ------------//

	val queryFrequency = freqRank(5)_ // set rank rounding
	//val queryResults = queryFrequency("Look, where they come")
	//val queryResults = queryFrequency("Take but good note")
	//val queryResults = queryFrequency	("pillar of the world")
	val queryResults = queryFrequency	("our dungy earth alike")
	//val queryResults = queryFrequency	("earth alike")
	
	queryResults foreach(
		queryResults => {
			println("----------------------------------")
			queryResults.values.foreach(insts => {
				println(insts.locs.docId + ", " + insts.locs.term + ", " + insts.rankMetric)
			})
		}
	)
  // Dependancy on preserved order of query in results
  // TODO: What about biword ranking?
  // What about assembling tuples of as much of the phrase as is found?
  // Need to couple terms with locations
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
  
  val biWordResults = biWord(queryResults)
  biWordResults foreach println
}
/*

 

*/