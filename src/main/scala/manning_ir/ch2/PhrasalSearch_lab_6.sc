package manning_ir.ch2

import manning_ir.ch2.PhrasalSearchLib_4._
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
object PhrasalSearch_lab_6 {
  	// Document path
	val root_path = "/Users/hieronymus/Development/Workspace_BAK/Intro IR Manning/"
                                                  //> root_path  : String = /Users/hieronymus/Development/Workspace_BAK/Intro IR M
                                                  //| anning/
  val doc_path = "src/main/resources/shakespeare/"//> doc_path  : String = src/main/resources/shakespeare/
  
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
  )                                               //> docList  : List[manning_ir.ch2.PhrasalSearchLib_4.Document] = List(Document
                                                  //| (100,Antony_and_Cleopatra.txt,William Shakespear,0,TreeSet()), Document(101
                                                  //| ,Hamlet.txt,William Shakespear,0,TreeSet()), Document(102,Julius_Caesar.txt
                                                  //| ,William Shakespear,0,TreeSet()), Document(103,Macbeth.txt,William Shakespe
                                                  //| ar,0,TreeSet()), Document(104,Othello.txt,William Shakespear,0,TreeSet()), 
                                                  //| Document(105,The_Tempest.txt,William Shakespear,0,TreeSet()))
  
  // Put docs in Catalog (i.e. Map)
  docList.foreach(doc => docCatalog += (doc.id -> doc))
  
  // ------ Indexing ------------------- //
  
  // partial set up with doc paths
  val index = indexDocument(root_path + doc_path)_//> index  : manning_ir.ch2.PhrasalSearchLib_4.Document => Unit = <function1>
  
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
	
	case class QueryResult(
		val docId: Long,
		val queryTerm: String,
		val rankMetric: Double
	)
	object QueryOrdering extends Ordering[QueryResult] {
		def compare(a: QueryResult, b: QueryResult) = a.rankMetric compare b.rankMetric
	}
	
	val round = roundAt(4)_                   //> round  : Double => Double = <function1>
	
	/* TODO : Sort QueryResults by ranking (tf).
	 */
	def phrasalQuery(query: String): List[List[QueryResult]] = {
		// tokenize query
		val qTerms = preprocess(query)
		qTerms foreach println; println("---------------------")
		
		// Disjunction of query terms, initial step
		val termsRetrieved = qTerms.map(dictionary.get(_)).flatten
		
		// TODO: convert to for comprehension. Put needed Option match on docCatalog.get in yield
		termsRetrieved.map( term => {
			term.incidences.values.map( incidences => {
				val docWordCount = docCatalog.get(incidences.docId).get.tokensTotal
				val tf = round(incidences.termCount.toDouble / docWordCount.toDouble)
				QueryResult(incidences.docId, incidences.term, tf)
			}).toList.sorted(QueryOrdering)
		}).toList
	} // end phrasalQuery                     //> phrasalQuery: (query: String)List[List[manning_ir.ch2.PhrasalSearch_lab_6.Q
                                                  //| ueryResult]]

	
	val queryResults = phrasalQuery("And, in conclusion, Elvis")
                                                  //> and
                                                  //| in
                                                  //| conclusion
                                                  //| elvis
                                                  //| ---------------------
                                                  //| queryResults  : List[List[manning_ir.ch2.PhrasalSearch_lab_6.QueryResult]] 
                                                  //| = List(List(QueryResult(100,and,0.0247), QueryResult(104,and,0.0282), Query
                                                  //| Result(105,and,0.0294), QueryResult(101,and,0.0298), QueryResult(102,and,0.
                                                  //| 0304), QueryResult(103,and,0.0309)), List(QueryResult(105,in,0.0092), Query
                                                  //| Result(100,in,0.0097), QueryResult(102,in,0.0108), QueryResult(103,in,0.011
                                                  //| 2), QueryResult(104,in,0.0119), QueryResult(101,in,0.0135)), List(QueryResu
                                                  //| lt(100,conclusion,0.0), QueryResult(104,conclusion,1.0E-4), QueryResult(103
                                                  //| ,conclusion,1.0E-4)))
	
	queryResults foreach(
		queryResults => {
			println("----------------------------------")
			queryResults foreach println
		}
	)                                         //> ----------------------------------
                                                  //| QueryResult(100,and,0.0247)
                                                  //| QueryResult(104,and,0.0282)
                                                  //| QueryResult(105,and,0.0294)
                                                  //| QueryResult(101,and,0.0298)
                                                  //| QueryResult(102,and,0.0304)
                                                  //| QueryResult(103,and,0.0309)
                                                  //| ----------------------------------
                                                  //| QueryResult(105,in,0.0092)
                                                  //| QueryResult(100,in,0.0097)
                                                  //| QueryResult(102,in,0.0108)
                                                  //| QueryResult(103,in,0.0112)
                                                  //| QueryResult(104,in,0.0119)
                                                  //| QueryResult(101,in,0.0135)
                                                  //| ----------------------------------
                                                  //| QueryResult(100,conclusion,0.0)
                                                  //| QueryResult(104,conclusion,1.0E-4)
                                                  //| QueryResult(103,conclusion,1.0E-4)
}
/*

 

*/