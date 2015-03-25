package manning_ir.ch1
 
import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.SortedSet

object Indexing_lab_1 {

	case class Document(
		val id: Int,
		val title: String,
		val author: String,
		val path: String,
		val tokensTotal: Option[Int] = None,
		val tokensUnique: Option[Int] = None
	)
	
	val dictionary = Map.empty[String, SortedSet[Int]]
                                                  //> dictionary  : scala.collection.mutable.Map[String,scala.collection.mutable.S
                                                  //| ortedSet[Int]] = Map()
  // ----------------------------------------- //
  
  val root_path = "/Users/hieronymus/Development/Workspace_BAK/Intro IR Manning/"
                                                  //> root_path  : String = /Users/hieronymus/Development/Workspace_BAK/Intro IR M
                                                  //| anning/
  val antAndCleo = Document(
 		100,
 		"Antony and Cleopatra",
 		"William Shakespear",
		"src/main/resources/shakespeare/Antony_and_Cleopatra.txt"
 	)                                         //> antAndCleo  : manning_ir.ch1.Indexing_lab_1.Document = Document(100,Antony a
                                                  //| nd Cleopatra,William Shakespear,src/main/resources/shakespeare/Antony_and_Cl
                                                  //| eopatra.txt,None,None)
  // -------------------------------------- //
  
	for (line <- Source.fromFile(root_path + antAndCleo.path).getLines) {
		val tokens = line.split("[ !,.]+") //("\\s+") // split on any number of contiguous space characters
		
		tokens.map(term => dictionary.get(term) match {
			case Some(posting) => posting += antAndCleo.id
	  		case None => {
	  			val posting = SortedSet.empty[Int]
	  			posting += antAndCleo.id
	  			dictionary += (term -> posting)
	  		}
		})
	}
	
	dictionary.keys.size                      //> res0: Int = 5351
}
/*




*/