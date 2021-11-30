package lab4

import lab1.LexData.lexAnalysis
import lab3.RPN

import scala.collection.mutable

object Main extends App{
  val source = scala.io.Source.fromFile("src/test/resources/test3.txt")
  val str = try source.mkString finally source.close()  // Строка исходного кода
  val data = lexAnalysis(str)  // Лексический данные
  val rpn = new RPN(data)  // Анализ с созданием ПОЛИЗ
  rpn.syntaxAnalysis()
//  data.printLexData()
//  println(rpn.ptList.list)
  val map = mutable.Map(0->0, 1->5)
  val interpreter = new Interpreter(rpn, map)  //Создание интерпретатора
  interpreter.start()
}
