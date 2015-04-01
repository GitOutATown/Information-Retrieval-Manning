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
	val queryResults = queryFrequency("Upon a tawny")
                                                  //> queryResults  : List[List[manning_ir.ch2.PhrasalSearchLib_9.QueryResult]] =
                                                  //|  List(List(QueryResult(Incidences(101,upon,54,TreeSet(53, 985, 1211, 1385, 
                                                  //| 1938, 2943, 3018, 3060, 3214, 3393, 3536, 4050, 5682, 5921, 5991, 6696, 670
                                                  //| 8, 6777, 7721, 8623, 8786, 9255, 10335, 11413, 12294, 12803, 14214, 15673, 
                                                  //| 15728, 15927, 15933, 17207, 17566, 17676, 17772, 17850, 18163, 18193, 18286
                                                  //| , 18370, 19423, 19555, 19983, 20024, 22955, 25471, 26087, 27674, 28455, 286
                                                  //| 01, 28828, 31095, 31679, 32071)),0.00167), QueryResult(Incidences(104,upon,
                                                  //| 51,TreeSet(348, 527, 825, 1696, 1873, 2282, 2367, 3387, 3501, 3874, 4942, 5
                                                  //| 071, 5960, 5994, 6870, 7342, 8187, 8553, 8600, 9066, 10055, 10493, 12406, 1
                                                  //| 2543, 14734, 14843, 15229, 15552, 16031, 16427, 17827, 18863, 19353, 20495,
                                                  //|  20834, 21583, 21628, 21698, 21835, 23471, 24593, 24702, 24726, 26003, 2635
                                                  //| 6, 26733, 27050, 27086, 27638, 27913, 27980)),0.00182), QueryResult(Inciden
                                                  //| ces(100,upon,58,TreeSet
                                                  //| Output exceeds cutoff limit.
	
	queryResults foreach(
		queryResults => {
			println("----------------------------------")
			queryResults foreach(doc => println(doc.incidences.term + ", " + doc.incidences.docId + ", " + doc.rankMetric))
		}
	)                                         //> ----------------------------------
                                                  //| upon, 101, 0.00167
                                                  //| upon, 104, 0.00182
                                                  //| upon, 100, 0.00214
                                                  //| upon, 102, 0.00229
                                                  //| upon, 105, 0.0024
                                                  //| upon, 103, 0.00328
                                                  //| ----------------------------------
                                                  //| a, 100, 0.01216
                                                  //| a, 102, 0.01252
                                                  //| a, 103, 0.01354
                                                  //| a, 104, 0.01574
                                                  //| a, 101, 0.01631
                                                  //| a, 105, 0.01775
                                                  //| ----------------------------------
                                                  //| tawny, 100, 4.0E-5
                                                  //| tawny, 105, 6.0E-5
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
  
  val biWordResults = biWord(queryResults)        //> biWordResults  : List[List[(Long, Long)]] = List(List((348,349), (60,61), (
                                                  //| 15108,15109), (12246,12247)), List((61,62)))
  // TODO:
  // There's some promising sequential pairs here.
  // But what's with the empty list. I see 60,61,62 which is a three word series,
  // but the query has four sequential words. And when, in fact, I delete the
  // word I still get 60,61,62, which would look right. I'm glad I ran it with
  // four words!
  biWordResults foreach println                   //> List((348,349), (60,61), (15108,15109), (12246,12247))
                                                  //| List((61,62))
}
/*

 

*/