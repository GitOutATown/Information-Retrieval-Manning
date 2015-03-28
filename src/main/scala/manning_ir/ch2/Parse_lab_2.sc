package manning_ir.ch2

object Parse_lab_2 {
	val root_path = "/Users/hieronymus/Development/Workspace_BAK/Intro IR Manning/"
                                                  //> root_path  : String = /Users/hieronymus/Development/Workspace_BAK/Intro IR M
                                                  //| anning/
  val doc_path = "src/main/resources/shakespeare/"//> doc_path  : String = src/main/resources/shakespeare/
	
	val counter = 0                           //> counter  : Int = 0
	scala.io.Source.fromFile(root_path + doc_path + "Hamlet.txt")
  .getLines
  .flatMap(_.split("\\W+"))
  .foldLeft(counter){
  		(counter, word) => {
  			val countIncr = counter + 1
  			println("countIncr: " + countIncr)
  			countIncr
  		}
  }                                               //> countIncr: 1
                                                  //| countIncr: 2
                                                  //| countIncr: 3
                                                  //| countIncr: 4
                                                  //| countIncr: 5
                                                  //| countIncr: 6
                                                  //| countIncr: 7
                                                  //| countIncr: 8
                                                  //| countIncr: 9
                                                  //| countIncr: 10
                                                  //| countIncr: 11
                                                  //| countIncr: 12
                                                  //| countIncr: 13
                                                  //| countIncr: 14
                                                  //| countIncr: 15
                                                  //| countIncr: 16
                                                  //| countIncr: 17
                                                  //| countIncr: 18
                                                  //| countIncr: 19
                                                  //| countIncr: 20
                                                  //| countIncr: 21
                                                  //| countIncr: 22
                                                  //| countIncr: 23
                                                  //| countIncr: 24
                                                  //| countIncr: 25
                                                  //| countIncr: 26
                                                  //| countIncr: 27
                                                  //| countIncr: 28
                                                  //| countIncr: 29
                                                  //| countIncr: 30
                                                  //| countIncr: 31
                                                  //| countIncr: 32
                                                  //| countIncr: 33
                                                  //| countIncr: 34
                                                  //| countIncr: 35
                                                  //| countIncr: 36
                                                  //| countIncr: 37
                                                  //| countIncr: 38
                                                  //| countIncr: 39
                                                  //| countIncr: 40
                                                  //| countIncr: 41
                                                  //| countIncr: 42
                                                  //| countIncr: 43
                                                  //| countIncr: 44
                                                  //| countIncr: 45
                                                  //| countIncr: 46
                                                  //| countIncr: 47
                                                  //| countIncr: 48
                                                  //| countIncr: 49
                                                  //| countIncr: 50
                                                  //| countIncr: 51
                                                  //| countIncr: 52
                                                  //| countIncr: 53
                                                  //| countIncr: 54
                                                  //| countIncr: 55
                                                  //| countIncr: 56
}