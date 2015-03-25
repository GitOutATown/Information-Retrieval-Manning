package manning_ir.ch1
 
import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.SortedSet

/*
 * Inspired by my reading of Christopher Manning's Introduction to Information Retrieval,
 * Ch 1, I'm coding a basic inverse index scheme for six plays of Shakespeare and some
 *	 test queries. At this point I'm not trying to optimize a production system, but rather
 * am just following a rough outline of the processes and features discussed.
 * For now, the postings record only a binary (Boolean) term occurence in the documents.
 * I will likely add term frequency when it is specifically addressed in the book.
 */
object Indexing_lab_1 {

	case class Document(
		val id: Int,
		val name: String,
		val author: String,
		var tokensTotal: Long = 0,
		var tokensUniqueSize: Option[Int] = None,
		var tokensUnique: SortedSet[String] = SortedSet.empty[String]
	)
	
	// TODO: I'm not sure that there is any optimization to be gained by using a SortedSet.
	// Manning discusses sorting as part of optimizing the algorithms he presents. But I'm
	// not sure it really translates to Scala. However, I'm sure there are other optimizations
	// that could or should be done for a production context.
	val dictionary = Map.empty[String, SortedSet[Int]]
                                                  //> dictionary  : scala.collection.mutable.Map[String,scala.collection.mutable.
                                                  //| SortedSet[Int]] = Map()
	// Document path
	val root_path = "/Users/hieronymus/Development/Workspace_BAK/Intro IR Manning/"
                                                  //> root_path  : String = /Users/hieronymus/Development/Workspace_BAK/Intro IR 
                                                  //| Manning/
  val doc_path = "src/main/resources/shakespeare/"//> doc_path  : String = src/main/resources/shakespeare/
	
	// Inverse index model: dictionary and postings. Postings are boolean occurance of term.
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
		  			val posting = SortedSet.empty[Int]
		  			posting += doc.id
		  			dictionary += (term -> posting)
		  			doc.tokensUnique += term
		  		}
			})
		}
  }                                               //> indexDocument: (doc: manning_ir.ch1.Indexing_lab_1.Document)Unit
	
	// Recursive
	def andFilter(xs0: List[List[Int]]): List[Int] = {
		xs0 match {
			case Nil => Nil
			case x :: Nil => x
			case x :: xs => andFilter(x.filter(xs.head.contains(_)) :: xs.tail)
		}
	}                                         //> andFilter: (xs0: List[List[Int]])List[Int]
	
	// Combines AND with OR postings
	def combAndOr(andPostings: List[Int], orPostings: List[Int]): List[Int] = {
		val combSet = collection.SortedSet(andPostings: _*) ++ orPostings
		combSet.toList
	}                                         //> combAndOr: (andPostings: List[Int], orPostings: List[Int])List[Int]
	
	// Filters out docs with unwanted terms
	def notFilter(andOrPostings: List[Int], notPostings: List[Int]): List[Int] = {
		andOrPostings.filter(!notPostings.contains(_))
	}                                         //> notFilter: (andOrPostings: List[Int], notPostings: List[Int])List[Int]
	
	// Rough implementation of AND, OR, and NOT using List params for each.
	def query(
		andTerms: Option[List[String]],
		orTerms: Option[List[String]],
		notTerms: Option[List[String]]): List[Int] = {
		
		// AND
		val andPostings = andTerms match {
			case Some(terms) => terms.map(dictionary.get(_).get.toList)
			case None => List.empty[List[Int]]
		}
		val andResult = andFilter(andPostings)
		println("andPostings: " + andPostings)
		println("andResult: " + andResult)
				
		// OR
		val orPostings = orTerms match {
			case Some(terms) => terms.flatMap(dictionary.get(_).get)
			case None => List.empty[Int]
		}
		val andOrResult = combAndOr(andResult, orPostings)
		println("orPostings: " + orPostings)
		println("andOrResult: " + andOrResult)
		
		// NOT
		val notPostings = notTerms match {
			case Some(terms) => terms.flatMap(dictionary.get(_).get)
			case None => List.empty[Int]
		}
		val notResult = notFilter(andOrResult, notPostings)
		println("notPostings: " + notPostings)
		println("notResult: " + notResult)
		
		notResult // final step in process chain
	} // end query                            //> query: (andTerms: Option[List[String]], orTerms: Option[List[String]], notT
                                                  //| erms: Option[List[String]])List[Int]
	
  // ------- Documents ---------------- //
    
  val antAndCleo = Document(
 		100,
 		"Antony_and_Cleopatra.txt",
 		"William Shakespear"
 	)                                         //> antAndCleo  : manning_ir.ch1.Indexing_lab_1.Document = Document(100,Antony_
                                                  //| and_Cleopatra.txt,William Shakespear,0,None,TreeSet())
 	val hamlet = Document(
 		101,
 		"Hamlet.txt",
 		"William Shakespear"
 	)                                         //> hamlet  : manning_ir.ch1.Indexing_lab_1.Document = Document(101,Hamlet.txt,
                                                  //| William Shakespear,0,None,TreeSet())
 	
 	val julius = Document(
 		102,
 		"Julius_Caesar.txt",
 		"William Shakespear"
 	)                                         //> julius  : manning_ir.ch1.Indexing_lab_1.Document = Document(102,Julius_Caes
                                                  //| ar.txt,William Shakespear,0,None,TreeSet())
 	
 	val macbeth = Document(
 		103,
 		"Macbeth.txt",
 		"William Shakespear"
 	)                                         //> macbeth  : manning_ir.ch1.Indexing_lab_1.Document = Document(103,Macbeth.tx
                                                  //| t,William Shakespear,0,None,TreeSet())
 	
 	val othello = Document(
 		104,
 		"Othello.txt",
 		"William Shakespear"
 	)                                         //> othello  : manning_ir.ch1.Indexing_lab_1.Document = Document(104,Othello.tx
                                                  //| t,William Shakespear,0,None,TreeSet())
 	
 	val tempest = Document(
 		105,
 		"The_Tempest.txt",
 		"William Shakespear"
 	)                                         //> tempest  : manning_ir.ch1.Indexing_lab_1.Document = Document(105,The_Tempes
                                                  //| t.txt,William Shakespear,0,None,TreeSet())
  
  
  
  val docs = List(antAndCleo, hamlet, julius, macbeth, othello, tempest).reverse
                                                  //> docs  : List[manning_ir.ch1.Indexing_lab_1.Document] = List(Document(105,Th
                                                  //| e_Tempest.txt,William Shakespear,0,None,TreeSet()), Document(104,Othello.tx
                                                  //| t,William Shakespear,0,None,TreeSet()), Document(103,Macbeth.txt,William Sh
                                                  //| akespear,0,None,TreeSet()), Document(102,Julius_Caesar.txt,William Shakespe
                                                  //| ar,0,None,TreeSet()), Document(101,Hamlet.txt,William Shakespear,0,None,Tre
                                                  //| eSet()), Document(100,Antony_and_Cleopatra.txt,William Shakespear,0,None,Tr
                                                  //| eeSet()))
  // ------ Indexing ------------------- //
  
  // Initiate indexing
  docs foreach indexDocument
	
	dictionary.keys.size                      //> res0: Int = 13743
	
	// ------ Logging -------------------- //
	
	// Indexing results
	docs foreach(doc => {
		println("------------------")
		println(doc.name)
		println(doc.tokensUnique.size)
		println(doc.tokensTotal)
	})                                        //> ------------------
                                                  //| The_Tempest.txt
                                                  //| 3809
                                                  //| 17468
                                                  //| ------------------
                                                  //| Othello.txt
                                                  //| 4613
                                                  //| 28026
                                                  //| ------------------
                                                  //| Macbeth.txt
                                                  //| 4031
                                                  //| 18314
                                                  //| ------------------
                                                  //| Julius_Caesar.txt
                                                  //| 3551
                                                  //| 20928
                                                  //| ------------------
                                                  //| Hamlet.txt
                                                  //| 5658
                                                  //| 32320
                                                  //| ------------------
                                                  //| Antony_and_Cleopatra.txt
                                                  //| 4775
                                                  //| 27137
  // ----- Queries ---------------- //
  
  dictionary.get("Antony")                        //> res1: Option[scala.collection.mutable.SortedSet[Int]] = Some(TreeSet(100, 1
                                                  //| 02))
	dictionary.get("Brutus")                  //> res2: Option[scala.collection.mutable.SortedSet[Int]] = Some(TreeSet(100, 1
                                                  //| 01, 102))
	dictionary.get("Caesar")                  //> res3: Option[scala.collection.mutable.SortedSet[Int]] = Some(TreeSet(100, 1
                                                  //| 01, 102, 103, 104))
	dictionary.get("Calpurnia")               //> res4: Option[scala.collection.mutable.SortedSet[Int]] = Some(TreeSet(102))
	dictionary.get("Cleopatra")               //> res5: Option[scala.collection.mutable.SortedSet[Int]] = Some(TreeSet(100))
	dictionary.get("mercy")                   //> res6: Option[scala.collection.mutable.SortedSet[Int]] = Some(TreeSet(100, 1
                                                  //| 01, 103, 104, 105))
	dictionary.get("worser")                  //> res7: Option[scala.collection.mutable.SortedSet[Int]] = Some(TreeSet(100, 1
                                                  //| 01, 104, 105))
	dictionary.get("Othello")                 //> res8: Option[scala.collection.mutable.SortedSet[Int]] = Some(TreeSet(104))
	
	// Initiate combined queries
	
	// ((Caesar AND Brutus) OR Othello) NOT Calpurnia
	val qRes1 = query(
		Option(List("Caesar", "Brutus")), // AND
		Option(List("Othello")), // OR
		Option(List("Calpurnia")) // NOT
	)                                         //> andPostings: List(List(100, 101, 102, 103, 104), List(100, 101, 102))
                                                  //| andResult: List(100, 101, 102)
                                                  //| orPostings: List(104)
                                                  //| andOrResult: List(100, 101, 102, 104)
                                                  //| notPostings: List(102)
                                                  //| notResult: List(100, 101, 104)
                                                  //| qRes1  : List[Int] = List(100, 101, 104)
  // (Caesar OR Brutus) NOT Calpurnia
  val qRes2 = query(None, Option(List("Caesar", "Brutus")), Option(List("Calpurnia")))
                                                  //> andPostings: List()
                                                  //| andResult: List()
                                                  //| orPostings: List(100, 101, 102, 103, 104, 100, 101, 102)
                                                  //| andOrResult: List(100, 101, 102, 103, 104)
                                                  //| notPostings: List(102)
                                                  //| notResult: List(100, 101, 103, 104)
                                                  //| qRes2  : List[Int] = List(100, 101, 103, 104)
  // (Caesar AND Brutus) NOT Calpurnia
  	val qRes3 = query(Option(List("Caesar", "Brutus")), None, Option(List("Calpurnia")))
                                                  //> andPostings: List(List(100, 101, 102, 103, 104), List(100, 101, 102))
                                                  //| andResult: List(100, 101, 102)
                                                  //| orPostings: List()
                                                  //| andOrResult: List(100, 101, 102)
                                                  //| notPostings: List(102)
                                                  //| notResult: List(100, 101)
                                                  //| qRes3  : List[Int] = List(100, 101)
  // (Caesar AND Brutus) OR Othello
  val qRes4 = query(Option(List("Caesar", "Brutus")), Option(List("Othello")), None)
                                                  //> andPostings: List(List(100, 101, 102, 103, 104), List(100, 101, 102))
                                                  //| andResult: List(100, 101, 102)
                                                  //| orPostings: List(104)
                                                  //| andOrResult: List(100, 101, 102, 104)
                                                  //| notPostings: List()
                                                  //| notResult: List(100, 101, 102, 104)
                                                  //| qRes4  : List[Int] = List(100, 101, 102, 104)
  
  
  
}
/*




*/