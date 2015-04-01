package manning_ir.ch2

object BiWord_lab_1 {

	// BiWords: (7,8), (58,59)
	val indexes1 = List(4, 7, 23, 27, 58, 99) //> indexes1  : List[Int] = List(4, 7, 23, 27, 58, 99)
	val indexes2 = List(8, 42, 59, 127)       //> indexes2  : List[Int] = List(8, 42, 59, 127)
	
	val result = for{
		left <- indexes1
		right = {
			val hit = indexes2.indexOf(left + 1)
			println("left:" + left + " hit:" + hit)
			hit
		}
		if right > -1
	} yield (left, indexes2(right))           //> left:4 hit:-1
                                                  //| left:7 hit:0
                                                  //| left:23 hit:-1
                                                  //| left:27 hit:-1
                                                  //| left:58 hit:2
                                                  //| left:99 hit:-1
                                                  //| result  : List[(Int, Int)] = List((7,8), (58,59))
}