package manning_ir.ch2

object Map_lab_1 {
	val countMap = Map.empty[String, Int]     //> countMap  : scala.collection.immutable.Map[String,Int] = Map()
	
	//countMap + (word -> (countMap.getOrElse(word, 0) + 1))
	countMap + ("huh" -> 1)                   //> res0: scala.collection.immutable.Map[String,Int] = Map(huh -> 1)
	countMap + ("foo" -> 2)                   //> res1: scala.collection.immutable.Map[String,Int] = Map(foo -> 2)
	countMap + ("mut" -> 3, "jef" -> 4)       //> res2: scala.collection.immutable.Map[String,Int] = Map(mut -> 3, jef -> 4)
	val word = "bar"                          //> word  : String = bar
	countMap + (word -> (countMap.getOrElse(word, 0) + 1))
  val huh = ("huh" -> 5)                          //> res3: scala.collection.immutable.Map[String,Int] = Map(bar -> 1)
	
}