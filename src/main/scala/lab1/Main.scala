package lab1

object Main extends App {
  //val data = LexData.lexAnalysis("do while 100<111 max = 66 / 33\nloop")
  val data = LexData.lexAnalysis("do while 1<0\n    max = (3 / 4) + (1-0)\nloop")
  data.printLexData()
  data.tokens.foreach(x => print(x.lexeme.c + " "))
}
