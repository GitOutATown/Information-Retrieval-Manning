package manning_ir.ch2

object Parse_lab_1 {

	val root_path = "/Users/hieronymus/Development/Workspace_BAK/Intro IR Manning/"
                                                  //> root_path  : String = /Users/hieronymus/Development/Workspace_BAK/Intro IR M
                                                  //| anning/
  val doc_path = "src/main/resources/shakespeare/"//> doc_path  : String = src/main/resources/shakespeare/

	scala.io.Source.fromFile(root_path + doc_path + "Hamlet.txt")
  .getLines
  .flatMap(_.split("\\W+"))
  .foldLeft(Map.empty[String, Int]){
     (countMap, word) => countMap + (word -> (countMap.getOrElse(word, 0) + 1))
  }                                               //> res0: scala.collection.immutable.Map[String,Int] = Map(serious -> 1, comply 
                                                  //| -> 2, breaks -> 2, forgotten -> 1, precious -> 2, E -> 3, hourly -> 1, lover
                                                  //|  -> 1, plentiful -> 1, lion -> 1, rate -> 2, mole -> 2, lights -> 3, spokes 
                                                  //| -> 2, rage -> 3, Much -> 1, snow -> 4, Plautus -> 1, mew -> 1, looks -> 7, C
                                                  //| lown -> 46, adders -> 1, easiness -> 2, blurs -> 1, exploit -> 1, swaggering
                                                  //|  -> 1, sorrows -> 1, Dies -> 5, rouse -> 3, craven -> 1, quietus -> 1, accid
                                                  //| ent -> 3, overcame -> 1, glean -> 1, didest -> 1, beneath -> 1, conjoin -> 1
                                                  //| , fang -> 1, gibes -> 1, frown -> 1, sweet -> 22, liquid -> 1, sword -> 16, 
                                                  //| used -> 2, observers -> 1, eye -> 15, couched -> 1, diadem -> 2, e -> 9, II 
                                                  //| -> 6, Still -> 4, MARCELLUS -> 40, altitude -> 1, wheaten -> 1, shipwrights 
                                                  //| -> 1, At -> 12, opinions -> 2, Stew -> 1, altogether -> 1, lasting -> 2, cra
                                                  //| fty -> 1, celestial -> 2, Devoutly -> 1, fashion -> 8, statists -> 1, tune -
                                                  //| > 2, drown -> 8, Belike 
                                                  //| Output exceeds cutoff limit.
}