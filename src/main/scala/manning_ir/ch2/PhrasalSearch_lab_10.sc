package manning_ir.ch2
 
import manning_ir.ch2.PhrasalSearchLib_9._
import util.math.Rounding_lab_1._

import scala.io.Source
import scala.collection.mutable.Map
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
object PhrasalSearch_lab_10 {
  	// Document path
	val root_path = "/Users/hieronymus/Development/Workspace_BAK/Intro IR Manning/"
                                                  //> root_path  : String = /Users/hieronymus/Development/Workspace_BAK/Intro IR M
                                                  //| anning/
  val doc_path = "src/main/resources/shakespeare/"//> doc_path  : String = src/main/resources/shakespeare/
	
	val docList = List[Document](
	  Document(IDGenerator.next, "Antony_and_Cleopatra.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Hamlet.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Julius_Caesar.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Macbeth.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "Othello.txt", "William Shakespear"),
	 	Document(IDGenerator.next, "The_Tempest.txt", "William Shakespear")
  )                                               //> docList  : List[manning_ir.ch2.PhrasalSearchLib_9.Document] = List(Document
                                                  //| (100,Antony_and_Cleopatra.txt,William Shakespear,0,TreeSet()), Document(101
                                                  //| ,Hamlet.txt,William Shakespear,0,TreeSet()), Document(102,Julius_Caesar.txt
                                                  //| ,William Shakespear,0,TreeSet()), Document(103,Macbeth.txt,William Shakespe
                                                  //| ar,0,TreeSet()), Document(104,Othello.txt,William Shakespear,0,TreeSet()), 
                                                  //| Document(105,The_Tempest.txt,William Shakespear,0,TreeSet()))
  
  // Put docs in Catalog (i.e. Map)
  docList.foreach(doc => docCatalog += (doc.id -> doc))
  
  // ------ Indexing ------------------- //
  
  // partial setup with doc paths
  val index = indexDocument(root_path + doc_path)_//> index  : manning_ir.ch2.PhrasalSearchLib_9.Document => Unit = <function1>
  
  // Initiate indexing
  docList foreach index
 
  	// ------ Logging indexing results ------ //
	
	dictionary.keys.size                      //> res0: Int = 12113
	
	/*
	docList foreach(doc => {
		println("------------------")
		println(doc.name)
		println("Unique terms: " + doc.tokensUnique.size)
		println("Word count: " + doc.tokensTotal)
	})
	*/
  
	// ----- Phrasal Querying ------------//

	val queryFrequency = freqRank(5)_ // set rank rounding
                                                  //> queryFrequency  : String => List[List[manning_ir.ch2.PhrasalSearchLib_9.Que
                                                  //| ryResult]] = <function1>
	val queryResults = queryFrequency("gipsy's lust")
                                                  //> queryResults  : List[List[manning_ir.ch2.PhrasalSearchLib_9.QueryResult]] =
                                                  //|  List(List(QueryResult(Incidences(100,gipsy's,1,TreeSet(95)),4.0E-5)), List
                                                  //| (QueryResult(Incidences(101,lust,2,TreeSet(5879, 5951)),6.0E-5), QueryResul
                                                  //| t(Incidences(105,lust,1,TreeSet(12539)),6.0E-5), QueryResult(Incidences(103
                                                  //| ,lust,2,TreeSet(13615, 13790)),1.1E-4), QueryResult(Incidences(104,lust,4,T
                                                  //| reeSet(5292, 8127, 8410, 11573)),1.4E-4), QueryResult(Incidences(100,lust,4
                                                  //| ,TreeSet(96, 4688, 13083, 13531)),1.5E-4)))
	
	queryResults foreach(
		queryResults => {
			println("----------------------------------")
			queryResults foreach(doc => println(doc.incidences.term + ", " + doc.incidences.docId + ", " + doc.rankMetric))
		}
	)                                         //> ----------------------------------
                                                  //| gipsy's, 100, 4.0E-5
                                                  //| ----------------------------------
                                                  //| lust, 101, 6.0E-5
                                                  //| lust, 105, 6.0E-5
                                                  //| lust, 103, 1.1E-4
                                                  //| lust, 104, 1.4E-4
                                                  //| lust, 100, 1.5E-4
  // Dependancy on preserved order of query in results
  // TODO: What about biword ranking?
  // What about assembling tuples of as much of the phrase as is found?
  // Need to couple terms with locations
  def biWord(queryTermResults: List[List[QueryResult]]): List[List[(Long, Long)]] = {
		
		def inter(terms: List[List[QueryResult]], accum: List[List[(Long, Long)]]): List[List[(Long, Long)]] = {
			terms match {
				case term :: Nil => accum
				case term :: xs => { // term is a List of QueryResults // A QueryResult contains all incidences of a single term within one document
					val result = for{
			  			doc <- term // each individual document
  						left <- doc.incidences.locs // each location of the term in this doc
  						rightIndex = {
  							// See if the next term after this term's location is the next term in the query
  							xs.head.head.incidences.locs.toList.indexOf(left + 1)
  						}
  						if rightIndex > -1 // if true, we found a sequential pair
			  		} yield {
			  			// TODO: I'm suspicious that there's something not right here.
			  			// This series of dots is ugly, and maybe not correct.
			  			// See comments at biWordResults below
			  			val right = xs.head.head.incidences.locs.toList(rightIndex)
			  			(left, right)
		  			}
			  		inter(xs, accum ++ List(result))
				} // end case x :: xs
			} // end terms match
		} // end inter
		
		inter(queryTermResults, List.empty[List[(Long, Long)]])
		
  } // end biWord                                 //> biWord: (queryTermResults: List[List[manning_ir.ch2.PhrasalSearchLib_9.Quer
                                                  //| yResult]])List[List[(Long, Long)]]
  
  val biWordResults = biWord(queryResults)        //> biWordResults  : List[List[(Long, Long)]] = List(List())
  // TODO:
  // There's some promising sequential pairs here.
  // But what's with the empty list. I see 60,61,62 which is a three word series,
  // but the query has four sequential words. And when, in fact, I delete the
  // word I still get 60,61,62, which would look right. I'm glad I ran it with
  // four words!
  // AFTER RUNNING MULTIPLE TESTS I'M GETTING TERRIBLE RESULTS!! LOT'S OF EMPTY LISTS--NOTHING AT ALL RETURNED!!
  biWordResults foreach println                   //> List()
}
/*

 

*/