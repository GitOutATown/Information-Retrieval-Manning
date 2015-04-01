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
object PhrasalSearch_lab_9 {
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
	val queryResults = queryFrequency("in conclusion, Elvis")
                                                  //> queryResults  : List[List[manning_ir.ch2.PhrasalSearchLib_9.QueryResult]] =
                                                  //|  List(List(QueryResult(Incidences(105,in,161,TreeSet(68, 220, 622, 669, 747
                                                  //| , 848, 855, 878, 1032, 1039, 1246, 1278, 1410, 1431, 1661, 1830, 1939, 2050
                                                  //| , 2096, 2266, 2271, 2282, 2378, 2464, 2490, 2501, 2526, 2532, 2774, 2832, 2
                                                  //| 842, 2949, 3029, 3040, 3107, 3214, 3259, 3264, 3309, 3532, 3572, 3733, 4088
                                                  //| , 4430, 4502, 4741, 4749, 4758, 4820, 4985, 5316, 5372, 5397, 5513, 5531, 5
                                                  //| 542, 5612, 5661, 5749, 5870, 5912, 5935, 6077, 6212, 6265, 6475, 6573, 6637
                                                  //| , 6875, 6929, 7086, 7275, 7289, 7601, 7640, 7686, 7803, 7949, 8228, 8407, 8
                                                  //| 424, 8728, 8777, 8844, 8921, 8937, 9112, 9174, 9486, 9564, 9646, 9655, 9688
                                                  //| , 10098, 10118, 10128, 10198, 10232, 10313, 10935, 11063, 11152, 11457, 115
                                                  //| 02, 11549, 11649, 11719, 11900, 12017, 12037, 12083, 12116, 12122, 12126, 1
                                                  //| 2162, 12611, 12907, 13163, 13216, 13400, 13413, 13480, 13506, 13731, 13838,
                                                  //|  13984, 14050, 14062, 1
                                                  //| Output exceeds cutoff limit.
	
	queryResults foreach(
		queryResults => {
			println("----------------------------------")
			queryResults foreach(doc => println(doc.incidences.term + ", " + doc.incidences.docId + ", " + doc.rankMetric))
		}
	)                                         //> ----------------------------------
                                                  //| in, 105, 0.00922
                                                  //| in, 100, 0.00973
                                                  //| in, 102, 0.0108
                                                  //| in, 103, 0.01119
                                                  //| in, 104, 0.01188
                                                  //| in, 101, 0.01346
                                                  //| ----------------------------------
                                                  //| conclusion, 100, 4.0E-5
                                                  //| conclusion, 103, 5.0E-5
                                                  //| conclusion, 104, 1.4E-4
  // Dependancy on preserved order of query in results
  // TODO: What about biword ranking?
  def biWord(queryTermResults: List[List[QueryResult]]) {
  		for{
  			term <- queryTermResults // each individual term
  			doc <- term // each individual document
  			location <- doc.incidences.locs
  		} yield {
  		
  		}
  }                                               //> biWord: (queryTermResults: List[List[manning_ir.ch2.PhrasalSearchLib_9.Quer
                                                  //| yResult]])Unit
  
  biWord(queryResults)
}
/*

 

*/