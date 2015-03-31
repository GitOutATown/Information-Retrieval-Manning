package manning_ir.ch2

import manning_ir.ch2.PhrasalSearchLib_4._
import util.math.Rounding_lab_1._

import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.SortedSet
import scala.math.log10

/**
 * Exercise in phrasal search. Not using POS or stop words.
 * Haven't yet been lowercasing, but I think it's time.
 * Using Term Frequency for ranking. Not using IDF because collection is too small.
 * Some open questions:
 * * Whether to use a sliding window strategy (on the query
 * * Basis for ranking
 * * Whether to use a proximity parameter on biword pairs (affects ranking)
 * * How to account for repeat terms in query
 */
object PhrasalSearch_lab_5 {
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
	
	/* TODO NEXT:
	 *	   Encapsulate document ranking results (maybe just a tuple)
	 *   Sort and print them.
	 */
	def phrasalQuery(query: String) {
		// tokenize query
		val qTerms = preprocess(query)
		
		// Disjunction of query terms, initial step
		val termsRetrieved = qTerms.map(qTerm => dictionary.get(qTerm))
		
		// ----- Diagnostic logging ------- //
		
		val round = roundAt(4)_
		
		qTerms foreach println
		println("---------------------")
		termsRetrieved foreach(term => term match {
			case Some(term) => {
				// http://www.tfidf.com/
				val termDocHits = term.incidences.size
				val docCatSize = docCatalog.size
				println(
					"Found term \"" + term.term + "\" in collection of size " + docCatSize +
					" with total count " + term.termCount +
					" in " + termDocHits + " docs:"
				)
				term.incidences.values.foreach( incidences => {
						val docTermCount = incidences.termCount
						val docWordCount = docCatalog.get(incidences.docId).get.tokensTotal
						val tf = round(docTermCount.toDouble / docWordCount.toDouble)
						println(
							"Document " + incidences.docId + " term count is " + docTermCount +
							" out of " + docWordCount + " total words for a TF of " + tf// +
							//"\n  For " + termDocHits + " document hits IDF is " +
							//log10((docCatSize.toDouble / termDocHits.toDouble) + 0.001)
						)
				})
				println("---------------------")
			}
			case None => // result None for query term
		})
	}                                         //> phrasalQuery: (query: String)Unit
	
	phrasalQuery("And, in conclusion. Elvis") //> and
                                                  //| in
                                                  //| conclusion
                                                  //| elvis
                                                  //| ---------------------
                                                  //| Found term "and" in collection of size 6 with total count 4139 in 6 docs:
                                                  //| Document 101 term count is 963 out of 32320 total words for a TF of 0.0298
                                                  //| Document 104 term count is 791 out of 28026 total words for a TF of 0.0282
                                                  //| Document 100 term count is 670 out of 27137 total words for a TF of 0.0247
                                                  //| Document 103 term count is 565 out of 18314 total words for a TF of 0.0309
                                                  //| Document 105 term count is 514 out of 17468 total words for a TF of 0.0294
                                                  //| Document 102 term count is 636 out of 20928 total words for a TF of 0.0304
                                                  //| ---------------------
                                                  //| Found term "in" in collection of size 6 with total count 1624 in 6 docs:
                                                  //| Document 101 term count is 435 out of 32320 total words for a TF of 0.0135
                                                  //| Document 104 term count is 333 out of 28026 total words for a TF of 0.0119
                                                  //| Document 100 term count is 264 out of 27137 total words for a TF of 0.0097
                                                  //| Document 103 term co
                                                  //| Output exceeds cutoff limit.
}
/*

 

*/