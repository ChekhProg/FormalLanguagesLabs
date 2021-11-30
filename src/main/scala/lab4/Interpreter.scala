package lab4

import lab3._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

class Interpreter(rpn: RPN,
                  idTable: mutable.Map[Int, Int] = mutable.Map.empty[Int, Int],
                  stack: ListBuffer[PostfixEntry] = ListBuffer.empty[PostfixEntry]) {
  def popVal(): Int = {
    val postfixEntry = stack.last
    val value = postfixEntry match {
      case EtVar(n) => idTable(n)
      case EtConst(n) => rpn.lexData.constTable(n).toInt
      case EtVal(n) => n
      case EtCmdPtr(n) => n
    }
    stack.remove(stack.size-1)
    value
  }
  def pushVal(v: Int): Unit = {
    stack.addOne(EtVal(v))
  }
  def pushElm(el: PostfixEntry): Unit = {
    stack.addOne(el)
  }
  def setVarAndPop(value: Int): Unit = {
    val postfixEntry = stack.last
    stack.remove(stack.size-1)
    postfixEntry match {
      case EtVar(n) => idTable(n) = value
      case _ => throw new Exception("Not variable")
    }
  }
  def start(): Unit = {
    var step = 0
    val list = rpn.ptList.list
    var pos = 0
    var tmp = 0
    var commandTmp = "None"
    println(f"Step $step%-3s| Command: $commandTmp%-15s | Stack: $stack%-50s| IdTable: ${idTable}")
    while (pos < list.size) {
      step += 1
      commandTmp = list(pos).toString
      list(pos) match {
        case EtCmd(cmd) =>
          cmd match {
            case JMP =>
              pos = popVal()
            case JZ =>
              tmp = popVal()
              if (popVal() == 1) pos += 1
              else pos = tmp
            case SET =>
              setVarAndPop(popVal())
              pos += 1
            case ADD =>
              val a = popVal()
              val b = popVal()
              pushVal(a + b)
              pos += 1
            case MUL =>
              val a = popVal()
              val b = popVal()
              pushVal(a * b)
              pos += 1
            case DIV =>
              val a = popVal()
              val b = popVal()
              pushVal(b / a)
              pos += 1
            case SUB =>
              val a = popVal()
              val b = popVal()
              pushVal(-a + b)
              pos += 1
            case AND =>
              val a = popVal()
              val b = popVal()
              val v = if (a == 1 && b == 1) 1 else 0
              pushVal(v)
              pos += 1
            case OR =>
              val a = popVal()
              val b = popVal()
              val v = if (a == 1 || b == 1) 1 else 0
              pushVal(v)
              pos += 1
            case CMPE =>
              val a = popVal()
              val b = popVal()
              val v = if (a == b) 1 else 0
              pushVal(v)
              pos += 1
            case CMPNE =>
              val a = popVal()
              val b = popVal()
              val v = if (a != b) 1 else 0
              pushVal(v)
              pos += 1
            case CMPL =>
              val a = popVal()
              val b = popVal()
              val v = if (a > b) 1 else 0
              pushVal(v)
              pos += 1
            case CMPG =>
              val a = popVal()
              val b = popVal()
              val v = if (a < b) 1 else 0
              pushVal(v)
              pos += 1
            case OUT =>
              val a = popVal()
              println("Output: " + a)
              pos += 1
            case INP =>
              val value = readLine("Input: ").toInt
              setVarAndPop(value)
              pos += 1
          }
        case _ =>
          pushElm(list(pos))
          pos += 1
      }
      println(f"Step $step%-3s| Command: $commandTmp%-15s | Stack: $stack%-50s| IdTable: ${idTable}")
    }
  }
}
