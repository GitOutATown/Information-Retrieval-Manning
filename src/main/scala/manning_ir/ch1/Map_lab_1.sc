package manning_ir.ch1

import scala.collection.mutable.Map
import scala.collection.mutable.SortedSet

object Map_lab_1 {
	
	val map = Map.empty[String, SortedSet[Int]]
                                                  //> map  : scala.collection.mutable.Map[String,scala.collection.mutable.SortedSe
                                                  //| t[Int]] = Map()
	
	val set = SortedSet.empty[Int]            //> set  : scala.collection.mutable.SortedSet[Int] = TreeSet()
	
	set.+=(3, 2, 1)                           //> res0: manning_ir.ch1.Map_lab_1.set.type = TreeSet(1, 2, 3)
	
	set.+=(7, 5)                              //> res1: manning_ir.ch1.Map_lab_1.set.type = TreeSet(1, 2, 3, 5, 7)
	
	map.+=("foo" -> set)                      //> res2: manning_ir.ch1.Map_lab_1.map.type = Map(foo -> TreeSet(1, 2, 3, 5, 7))
                                                  //| 
	map.get("foo").get                        //> res3: scala.collection.mutable.SortedSet[Int] = TreeSet(1, 2, 3, 5, 7)
	map.get("bar")                            //> res4: Option[scala.collection.mutable.SortedSet[Int]] = None
	
}