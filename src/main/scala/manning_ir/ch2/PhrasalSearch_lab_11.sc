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
object PhrasalSearch_lab_11 {
  	// Document path
	val root_path = "/Users/hieronymus/Development/Workspace_BAK/Intro IR Manning/"
                                                  //> root_path  : String = /Users/hieronymus/Development/Workspace_BAK/Intro IR M
                                                  //| anning/
	val doc_path = "src/main/resources/shakespeare/"
                                                  //> doc_path  : String = src/main/resources/shakespeare/
	
	val docList = List[Document](
		Document(IDGenerator.next, "Antony_and_Cleopatra.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Hamlet.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Julius_Caesar.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Macbeth.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Othello.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "The_Tempest.txt", "William Shakespear")
	)                                         //> docList  : List[manning_ir.ch2.PhrasalSearchLib_10.Document] = List(Documen
                                                  //| t(100,Antony_and_Cleopatra.txt,William Shakespear,0,TreeSet()), Document(10
                                                  //| 1,Hamlet.txt,William Shakespear,0,TreeSet()), Document(102,Julius_Caesar.tx
                                                  //| t,William Shakespear,0,TreeSet()), Document(103,Macbeth.txt,William Shakesp
                                                  //| ear,0,TreeSet()), Document(104,Othello.txt,William Shakespear,0,TreeSet()),
                                                  //|  Document(105,The_Tempest.txt,William Shakespear,0,TreeSet()))
  
	// Put docs in Catalog (i.e. Map)
	docList.foreach(doc => docCatalog += (doc.id -> doc))
  
	// ------ Indexing ------------------- //
  
	// partial setup with doc paths
	val index = indexDocument(root_path + doc_path)_
                                                  //> index  : manning_ir.ch2.PhrasalSearchLib_10.Document => Unit = <function1>
  
	// Initiate indexing
	docList foreach index
 
  	// ------ Logging indexing results ------ //
	
	dictionary.keys.size                      //> res0: Int = 12113
	
	docList foreach(doc => {
		println("------------------")
		println(doc.name)
		println("Unique terms: " + doc.tokensUnique.size)
		println("Word count: " + doc.tokensTotal)
	})                                        //> ------------------
                                                  //| Antony_and_Cleopatra.txt
                                                  //| Unique terms: 4218
                                                  //| Word count: 27137
                                                  //| ------------------
                                                  //| Hamlet.txt
                                                  //| Unique terms: 5142
                                                  //| Word count: 32320
                                                  //| ------------------
                                                  //| Julius_Caesar.txt
                                                  //| Unique terms: 3123
                                                  //| Word count: 20928
                                                  //| ------------------
                                                  //| Macbeth.txt
                                                  //| Unique terms: 3596
                                                  //| Word count: 18314
                                                  //| ------------------
                                                  //| Othello.txt
                                                  //| Unique terms: 4164
                                                  //| Word count: 28026
                                                  //| ------------------
                                                  //| The_Tempest.txt
                                                  //| Unique terms: 3423
                                                  //| Word count: 17468
  
	// ----- Phrasal Querying ------------//

	val queryFrequency = freqRank(5)_ // set rank rounding
                                                  //> queryFrequency  : String => List[scala.collection.immutable.Map[Long,mannin
                                                  //| g_ir.ch2.PhrasalSearchLib_10.Incidences]] = <function1>
	//val queryResults = queryFrequency("Look, where they come")
	//val queryResults = queryFrequency("Take but good note")
	//val queryResults = queryFrequency	("pillar of the world")
	val queryResults = queryFrequency	("our dungy earth alike")
                                                  //> queryResults  : List[scala.collection.immutable.Map[Long,manning_ir.ch2.Phr
                                                  //| asalSearchLib_10.Incidences]] = List(Map(101 -> Incidences(Locations(101,ou
                                                  //| r,118,TreeSet(201, 244, 273, 555, 582, 674, 707, 715, 761, 838, 869, 875, 9
                                                  //| 10, 1026, 1187, 1289, 1366, 1409, 1412, 1462, 1477, 1482, 1515, 1519, 1582,
                                                  //|  1596, 1601, 1606, 1647, 1770, 2275, 2356, 2393, 2410, 2412, 2417, 3276, 33
                                                  //| 00, 3547, 4906, 4913, 4924, 5180, 5188, 6767, 7958, 8037, 8044, 8126, 8206,
                                                  //|  8244, 8299, 8351, 8400, 8403, 8587, 8612, 8624, 8777, 10307, 10311, 10507,
                                                  //|  10513, 10519, 13225, 14085, 14564, 15496, 15561, 16112, 16187, 16192, 1652
                                                  //| 6, 16530, 16603, 16612, 16617, 16625, 16875, 17523, 18124, 18616, 18780, 20
                                                  //| 979, 21076, 21144, 21855, 21987, 22093, 23312, 23581, 23877, 24240, 24243, 
                                                  //| 24245, 24523, 24950, 25973, 25981, 25985, 26068, 26143, 26269, 27558, 28721
                                                  //| , 28840, 28879, 28956, 28963, 28978, 29155, 29318, 29444, 29912, 30157, 312
                                                  //| 77, 32011, 32044)),0.00
                                                  //| Output exceeds cutoff limit.
	//val queryResults = queryFrequency	("earth alike")
	//val queryResults = queryFrequency	("And, in conclusion, Elvis")
	
	queryResults foreach(
		queryResults => {
			println("----------------------------------")
			queryResults.values.foreach(insts => {
				println(insts.locs.docId + ", " + insts.locs.term + ", " + insts.rankMetric)
			})
		}
	)                                         //> ----------------------------------
                                                  //| 101, our, 0.00365
                                                  //| 102, our, 0.0042
                                                  //| 105, our, 0.00292
                                                  //| 103, our, 0.00644
                                                  //| 104, our, 0.00182
                                                  //| 100, our, 0.00505
                                                  //| ----------------------------------
                                                  //| 100, dungy, 4.0E-5
                                                  //| ----------------------------------
                                                  //| 101, earth, 7.1E-4
                                                  //| 102, earth, 2.4E-4
                                                  //| 105, earth, 5.2E-4
                                                  //| 103, earth, 3.8E-4
                                                  //| 104, earth, 1.4E-4
                                                  //| 100, earth, 4.8E-4
                                                  //| ----------------------------------
                                                  //| 100, alike, 1.8E-4
                                                  //| 103, alike, 5.0E-5
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
		
  } // end biWord                                 //> biWord: (queryTermResults: manning_ir.ch2.PhrasalSearchLib_10.QueryTermResu
                                                  //| lts)List[List[manning_ir.ch2.PhrasalSearchLib_10.BiWord]]
  
  val biWordResults = biWord(queryResults)        //> biWordResults  : List[List[manning_ir.ch2.PhrasalSearchLib_10.BiWord]] = Li
                                                  //| st(List(BiWord(100,331,332,our,dungy)), List(BiWord(100,332,333,dungy,earth
                                                  //| )), List(BiWord(100,333,334,earth,alike)))
  biWordResults foreach println                   //> List(BiWord(100,331,332,our,dungy))
                                                  //| List(BiWord(100,332,333,dungy,earth))
                                                  //| List(BiWord(100,333,334,earth,alike))
}
/*

 

*/