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
  // ------- Documents ---------------------------------- //
    
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
  // ------ Indexing ------------------- //
  
  val root_path = "/Users/hieronymus/Development/Workspace_BAK/Intro IR Manning/"
                                                  //> root_path  : String = /Users/hieronymus/Development/Workspace_BAK/Intro IR 
                                                  //| Manning/
  val doc_path = "src/main/resources/shakespeare/"//> doc_path  : String = src/main/resources/shakespeare/
  
  
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
		} // end for line
  } // end indexDocument                          //> indexDocument: (doc: manning_ir.ch1.Indexing_lab_1.Document)Unit
  
  val docs = List(antAndCleo, hamlet, julius, macbeth, othello, tempest)
                                                  //> docs  : List[manning_ir.ch1.Indexing_lab_1.Document] = List(Document(100,An
                                                  //| tony_and_Cleopatra.txt,William Shakespear,0,None,TreeSet()), Document(101,H
                                                  //| amlet.txt,William Shakespear,0,None,TreeSet()), Document(102,Julius_Caesar.
                                                  //| txt,William Shakespear,0,None,TreeSet()), Document(103,Macbeth.txt,William 
                                                  //| Shakespear,0,None,TreeSet()), Document(104,Othello.txt,William Shakespear,0
                                                  //| ,None,TreeSet()), Document(105,The_Tempest.txt,William Shakespear,0,None,Tr
                                                  //| eeSet()))
  docs foreach indexDocument
	
	dictionary.keys.size                      //> res0: Int = 13743
	
	// Logging
	docs foreach(doc => {
		println("------------------")
		println(doc.name)
		println(doc.tokensUnique.size)
		println(doc.tokensTotal)
	})                                        //> ------------------
                                                  //| Antony_and_Cleopatra.txt
                                                  //| 4775
                                                  //| 27137
                                                  //| ------------------
                                                  //| Hamlet.txt
                                                  //| 5658
                                                  //| 32320
                                                  //| ------------------
                                                  //| Julius_Caesar.txt
                                                  //| 3551
                                                  //| 20928
                                                  //| ------------------
                                                  //| Macbeth.txt
                                                  //| 4031
                                                  //| 18314
                                                  //| ------------------
                                                  //| Othello.txt
                                                  //| 4613
                                                  //| 28026
                                                  //| ------------------
                                                  //| The_Tempest.txt
                                                  //| 3809
                                                  //| 17468
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
	// Recursive
	def andFilter(xs0: List[List[Int]]): List[Int] = {
		xs0 match {
			case x :: Nil => x
			case x :: xs => andFilter(x.filter(xs.head.contains(_)) :: xs.tail)
		}
	}                                         //> andFilter: (xs0: List[List[Int]])List[Int]
	
	def query(
		andTerms: Option[List[String]],
		orTerms: Option[List[String]],
		notTerms: Option[List[String]]) {
		
		val andPostings = andTerms match {
			case Some(terms) => terms.map(dictionary.get(_).get.toList)
			case None => List.empty[List[Int]]
		}
		val andResult = andFilter(andPostings)
		println("andPostings: " + andPostings)
		println("andResult: " + andResult)
		
		val orPostings = orTerms match {
			case Some(terms) => terms.map(dictionary.get(_).get.toList)
			case None =>
		}
		println("orPostings: " + orPostings)
		
		val notPostings = notTerms match {
			case Some(terms) => terms.map(dictionary.get(_).get.toList)
			case None =>
		}
		println("notPostings: " + notPostings)
		
	} // end query                            //> query: (andTerms: Option[List[String]], orTerms: Option[List[String]], notT
                                                  //| erms: Option[List[String]])Unit
	
	query(Option(List("Caesar", "Brutus")), None, None)
                                                  //> andPostings: List(List(100, 101, 102, 103, 104), List(100, 101, 102))
                                                  //| andResult: List(100, 101, 102)
                                                  //| orPostings: ()
                                                  //| notPostings: ()
}
/*




*/