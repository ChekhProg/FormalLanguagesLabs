package lab3

import lab1.LexData.lexAnalysis

object Main extends App {
  val source = scala.io.Source.fromFile("src/test/resources/test3.txt")
  val str = try source.mkString finally source.close()
  val data = lexAnalysis(str)
  val rpn = new RPN(data)
  println(rpn.syntaxAnalysis())
  rpn.ptList.list.foreach(x => println(x))
  //fromFile()
  // do while 1<0 or 2<>3 m = 3 / 4; s = (3 + i) + (1 /2); output m loop
  // do while 100<111 max = (66 / 33)\n loop
}
