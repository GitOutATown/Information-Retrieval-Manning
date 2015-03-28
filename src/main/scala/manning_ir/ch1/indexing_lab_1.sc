package manning_ir.ch1
 
import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.SortedSet

/*
 * Christopher Manning's Introduction to Information Retrieval, Ch.1
 * This is a basic inverse index scheme for six Shakespeare plays and example
 *	 test term queries using AND, OR, NOT. I'm not attempting any optimization,
 * just a basic outline of processes and features discussed.
 * The postings in Ch.1 are binary (Boolean) term occurence in the documents.
 * Term frequency comes in Ch.2.
 */
object Indexing_lab_1 {

	case class Document(
		val id: Long,
		val name: String,
		val author: String,
		var tokensTotal: Long = 0,
		var tokensUniqueSize: Option[Long] = None,
		var tokensUnique: SortedSet[String] = SortedSet.empty[String]
	)
	
	// I'm not sure that there is any optimization to be gained by using a SortedSet.
	// Manning discusses sorting as part of optimizing the algorithms he presents.
	// But I'm not sure it translates to Scala. There are, however, other likely
	// optimizations, but that is not the current focus here.
	val dictionary = Map.empty[String, SortedSet[Long]]
                                                  //> dictionary  : scala.collection.mutable.Map[String,scala.collection.mutable.
                                                  //| SortedSet[Long]] = Map()
	
	// Document catalog
	val docCatalog = Map.empty[Long, Document]//> docCatalog  : scala.collection.mutable.Map[Long,manning_ir.ch1.Indexing_lab
                                                  //| _1.Document] = Map()
	// Document path
	val root_path = "/Users/hieronymus/Development/Workspace_BAK/Intro IR Manning/"
                                                  //> root_path  : String = /Users/hieronymus/Development/Workspace_BAK/Intro IR 
                                                  //| Manning/
  val doc_path = "src/main/resources/shakespeare/"//> doc_path  : String = src/main/resources/shakespeare/
	
	// Inverse index model: dictionary of term postings. Postings are boolean occurance of term within a document.
  def indexDocument(doc: Document) {
  		for (line <- Source.fromFile(root_path + doc_path + doc.name).getLines) {
			val tokens = line.split("[ !,.:;]+") //("\\s+") // split on any number of contiguous space characters
			doc.tokensTotal += tokens.length
			
			tokens.map(term => dictionary.get(term) match {
				case Some(posting) => {
					posting += doc.id
					doc.tokensUnique += term
				}
		  		case None => {
		  			val posting = SortedSet.empty[Long]
		  			posting += doc.id
		  			dictionary += (term -> posting)
		  			doc.tokensUnique += term
		  		}
			})
		}
  }                                               //> indexDocument: (doc: manning_ir.ch1.Indexing_lab_1.Document)Unit
  
  // Query combiners AND, OR, NOT
	
	// Conjunction, i.e. AND, recursive merging
	def andFilter(xs0: List[List[Long]]): List[Long] = {
		xs0 match {
			case Nil => Nil
			case x :: Nil => x
			case x :: xs => andFilter(x.filter(xs.head.contains(_)) :: xs.tail)
		}
	}                                         //> andFilter: (xs0: List[List[Long]])List[Long]
	
	// Combines AND with OR postings, i.e. disjunction
	def combAndOr(andPostings: List[Long], orPostings: List[Long]): List[Long] = {
		val combSet = collection.SortedSet(andPostings: _*) ++ orPostings
		combSet.toList
	}                                         //> combAndOr: (andPostings: List[Long], orPostings: List[Long])List[Long]
	
	// Filters out docs with unwanted terms, i.e. NOT
	def notFilter(andOrPostings: List[Long], notPostings: List[Long]): List[Long] = {
		andOrPostings.filter(!notPostings.contains(_))
	}                                         //> notFilter: (andOrPostings: List[Long], notPostings: List[Long])List[Long]

	// Query implementation of AND, OR, and NOT uses List params for each.
	def query(
		andTerms: Option[List[String]],
		orTerms: Option[List[String]],
		notTerms: Option[List[String]]): List[Long] = {
		
		// AND
		val andPostings = andTerms match {
			case Some(terms) => terms.map(dictionary.get(_).get.toList)
			case None => List.empty[List[Long]]
		}
		val andResult = andFilter(andPostings)
		println("andPostings: " + andPostings)
		println("andResult: " + andResult)
				
		// OR
		val orPostings = orTerms match {
			case Some(terms) => terms.flatMap(dictionary.get(_).get)
			case None => List.empty[Long]
		}
		val andOrResult = combAndOr(andResult, orPostings)
		println("orPostings: " + orPostings)
		println("andOrResult: " + andOrResult)
		
		// NOT
		val notPostings = notTerms match {
			case Some(terms) => terms.flatMap(dictionary.get(_).get)
			case None => List.empty[Long]
		}
		val andOrNotResult = notFilter(andOrResult, notPostings)
		println("notPostings: " + notPostings)
		println("notResult: " + andOrNotResult)
		
		andOrNotResult // final step in process chain
	} // end query                            //> query: (andTerms: Option[List[String]], orTerms: Option[List[String]], notT
                                                  //| erms: Option[List[String]])List[Long]
	
  // ------- Documents ---------------- //
  
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
  )                                               //> docList  : List[manning_ir.ch1.Indexing_lab_1.Document] = List(Document(100
                                                  //| ,Antony_and_Cleopatra.txt,William Shakespear,0,None,TreeSet()), Document(10
                                                  //| 1,Hamlet.txt,William Shakespear,0,None,TreeSet()), Document(102,Julius_Caes
                                                  //| ar.txt,William Shakespear,0,None,TreeSet()), Document(103,Macbeth.txt,Willi
                                                  //| am Shakespear,0,None,TreeSet()), Document(104,Othello.txt,William Shakespea
                                                  //| r,0,None,TreeSet()), Document(105,The_Tempest.txt,William Shakespear,0,None
                                                  //| ,TreeSet()))
  
  // Put docs in Catalog (i.e. Map)
  docList.foreach(doc => docCatalog += (doc.id -> doc))
  
  // ------ Indexing ------------------- //
  
  // Initiate indexing
  docList foreach indexDocument
	
	// ------ Logging ndexing results ------ //
	
	dictionary.keys.size                      //> res0: Int = 13743
	
	docList foreach(doc => {
		println("------------------")
		println(doc.name)
		println("Unique terms: " + doc.tokensUnique.size)
		println("Word count: " + doc.tokensTotal)
	})                                        //> ------------------
                                                  //| Antony_and_Cleopatra.txt
                                                  //| Unique terms: 4775
                                                  //| Word count: 27137
                                                  //| ------------------
                                                  //| Hamlet.txt
                                                  //| Unique terms: 5658
                                                  //| Word count: 32320
                                                  //| ------------------
                                                  //| Julius_Caesar.txt
                                                  //| Unique terms: 3551
                                                  //| Word count: 20928
                                                  //| ------------------
                                                  //| Macbeth.txt
                                                  //| Unique terms: 4031
                                                  //| Word count: 18314
                                                  //| ------------------
                                                  //| Othello.txt
                                                  //| Unique terms: 4613
                                                  //| Word count: 28026
                                                  //| ------------------
                                                  //| The_Tempest.txt
                                                  //| Unique terms: 3809
                                                  //| Word count: 17468
  // ----- Queries ---------------- //
  
  dictionary.get("Antony")                        //> res1: Option[scala.collection.mutable.SortedSet[Long]] = Some(TreeSet(100, 
                                                  //| 102))
	dictionary.get("Brutus")                  //> res2: Option[scala.collection.mutable.SortedSet[Long]] = Some(TreeSet(100, 
                                                  //| 101, 102))
	dictionary.get("Caesar")                  //> res3: Option[scala.collection.mutable.SortedSet[Long]] = Some(TreeSet(100, 
                                                  //| 101, 102, 103, 104))
	dictionary.get("Calpurnia")               //> res4: Option[scala.collection.mutable.SortedSet[Long]] = Some(TreeSet(102))
                                                  //| 
	dictionary.get("Cleopatra")               //> res5: Option[scala.collection.mutable.SortedSet[Long]] = Some(TreeSet(100))
                                                  //| 
	dictionary.get("mercy")                   //> res6: Option[scala.collection.mutable.SortedSet[Long]] = Some(TreeSet(100, 
                                                  //| 101, 103, 104, 105))
	dictionary.get("worser")                  //> res7: Option[scala.collection.mutable.SortedSet[Long]] = Some(TreeSet(100, 
                                                  //| 101, 104, 105))
	dictionary.get("Othello")                 //> res8: Option[scala.collection.mutable.SortedSet[Long]] = Some(TreeSet(104))
                                                  //| 
	
	// Initiate combined queries
	
	// ((Caesar AND Brutus) OR Othello) NOT Calpurnia
	query(
		Option(List("Caesar", "Brutus")), // AND
		Option(List("Othello")), // OR
		Option(List("Calpurnia")) // NOT
	).map(docCatalog.get(_).get.name)         //> andPostings: List(List(100, 101, 102, 103, 104), List(100, 101, 102))
                                                  //| andResult: List(100, 101, 102)
                                                  //| orPostings: List(104)
                                                  //| andOrResult: List(100, 101, 102, 104)
                                                  //| notPostings: List(102)
                                                  //| notResult: List(100, 101, 104)
                                                  //| res9: List[String] = List(Antony_and_Cleopatra.txt, Hamlet.txt, Othello.txt
                                                  //| )
	
  // (Caesar OR Brutus) NOT Calpurnia
  query(
  		None,
  		Option(List("Caesar", "Brutus")),
  		Option(List("Calpurnia"))
  	).map(docCatalog.get(_).get.name)         //> andPostings: List()
                                                  //| andResult: List()
                                                  //| orPostings: List(100, 101, 102, 103, 104, 100, 101, 102)
                                                  //| andOrResult: List(100, 101, 102, 103, 104)
                                                  //| notPostings: List(102)
                                                  //| notResult: List(100, 101, 103, 104)
                                                  //| res10: List[String] = List(Antony_and_Cleopatra.txt, Hamlet.txt, Macbeth.tx
                                                  //| t, Othello.txt)
  // (Caesar AND Brutus) NOT Calpurnia
  	query(
  		Option(List("Caesar", "Brutus")),
  		None,
  		Option(List("Calpurnia"))
  	).map(docCatalog.get(_).get.name)         //> andPostings: List(List(100, 101, 102, 103, 104), List(100, 101, 102))
                                                  //| andResult: List(100, 101, 102)
                                                  //| orPostings: List()
                                                  //| andOrResult: List(100, 101, 102)
                                                  //| notPostings: List(102)
                                                  //| notResult: List(100, 101)
                                                  //| res11: List[String] = List(Antony_and_Cleopatra.txt, Hamlet.txt)
  // (Caesar AND Brutus) OR Othello
  query(
  		Option(List("Caesar", "Brutus")),
  		Option(List("Othello")),
  		None
  	).map(docCatalog.get(_).get.name)         //> andPostings: List(List(100, 101, 102, 103, 104), List(100, 101, 102))
                                                  //| andResult: List(100, 101, 102)
                                                  //| orPostings: List(104)
                                                  //| andOrResult: List(100, 101, 102, 104)
                                                  //| notPostings: List()
                                                  //| notResult: List(100, 101, 102, 104)
                                                  //| res12: List[String] = List(Antony_and_Cleopatra.txt, Hamlet.txt, Julius_Cae
                                                  //| sar.txt, Othello.txt)
}
/*




*/