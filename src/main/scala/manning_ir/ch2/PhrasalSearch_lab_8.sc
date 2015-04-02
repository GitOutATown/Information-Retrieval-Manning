package manning_ir.ch2
 
import manning_ir.ch2.PhrasalSearchLib_8._
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
object PhrasalSearch_lab_8 {
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

	val query = phrasalQuery(5)_ // set rank rounding
	val queryResults = query("And, in conclusion, Elvis")
	
	queryResults foreach(
		queryResults => {
			println("----------------------------------")
			queryResults foreach println
		}
	)
}
/*

 

*/