package lab3

import lab1._

import javax.lang.model.element.ModuleElement.DirectiveVisitor
import scala.collection.mutable.ListBuffer

class RPN(
           lexData: LexData,
           var iterator: Iterator[Token] = null,
           var token: Option[Token] = None,
           val ptList: PtList = new PtList()
         ) {
  def syntaxAnalysis(): Boolean = {
    iterator = lexData.tokens.iterator
    token = iterator.nextOption()
    whileStatement()
  }

  private def error(msg: String): Unit = {
    println(msg)
  }

  private def whileStatement(): Boolean = {

    val indStartCycle = ptList.size() // Адрес начала цикла

    token match {
      case None =>
        error("Ожидается 'do'. Получено ничего")
        return false
      case Some(value) if value.tokenName != TokenDo =>
        error("Ожидается 'do'. Получено " + value.tokenName + ", pos: " + value.pos)
        return false
      case Some(_) => ()
    }
    token = iterator.nextOption()
    token match {
      case None =>
        error("Ожидается 'while'. Получено ничего")
        return false
      case Some(value) if value.tokenName != TokenWhile =>
        error("Ожидается 'while'. Получено " + value.tokenName + ", pos: " + value.pos)
        return false
      case Some(_) => ()
    }
    token = iterator.nextOption()

    if (!condition()) return false

    // сформирована часть ПОЛИЗа, вычисляющая условие цикла

    val indEndCycle = ptList.writeCmdPtr(-1) // Адрес условного перехода после цикла
    ptList.writeCmd(JZ) // Запись команды перехода

    if (!statements()) return false

    // сформирована часть ПОЛИЗа для тела цикла

    token match {
      case None =>
        error("Ожидается 'loop'. Получено ничего")
        return false
      case Some(value) if value.tokenName != TokenLoop =>
        error("Ожидается 'loop'. Получено " + value.tokenName + ", pos: " + value.pos)
        return false
      case Some(_) => ()
    }

    ptList.writeCmdPtr(indStartCycle) //заносим адрес начала цикла
    val indLast = ptList.writeCmd(JMP) // заносим команду безусловного
    ptList.setCmdPtr(indEndCycle, indLast + 1) //изменяем фиктивное значение

    true
  }

  private def condition(): Boolean = {
    if (!logExpr()) return false
    // сформирована часть ПОЛИЗа для вычисления логического подвыражения
    while (token.isDefined && token.get.tokenName == TokenOr) {
      token = iterator.nextOption()
      if (!logExpr()) return false
      // сформирована часть ПОЛИЗа для вычисления логического подвыражения
      ptList.writeCmd(OR); //заносим операцию OR в ПОЛИЗ
    }
    true
  }

  private def logExpr(): Boolean = {
    if (!relExpr()) return false
    // сформирована часть ПОЛИЗа для вычисления подвыражения сравнения
    while (token.isDefined && token.get.tokenName == TokenAnd) {
      token = iterator.nextOption()
      if (!relExpr()) return false
      // сформирована часть ПОЛИЗа для вычисления подвыражения сравнения
      ptList.writeCmd(AND); //заносим операцию AND в ПОЛИЗ
    }
    true
  }

  private def relExpr(): Boolean = {
    if (!operand()) return false
    if (token.isDefined && (token.get.tokenName == TokenRel || token.get.tokenName == TokenEq)) {

      val cmd = token.get.lexeme.c match { // определяем команду
        case "<>" => CMPNE
        case "<" => CMPL
        case ">" => CMPG
        case "=" => CMPE
      }

      token = iterator.nextOption()
      if (!operand()) return false

      ptList.writeCmd(cmd) // заносим операцию в ПОЛИЗ
    }
    true
  }

  private def operand(): Boolean = {
    token match {
      case None =>
        error("Ожидается переменная или константа. Получено ничего")
        false
      case Some(value) if value.tokenName != TokenId && value.tokenName != TokenConst =>
        error("Ожидается переменная или константа. Получено " + value.tokenName + ", pos: " + value.pos)
        false
      case Some(value) =>
        value match {
          case Token(TokenId, Id(_, n), _, _) => ptList.writeVar(n)  // тип лексемы – переменная
          case Token(TokenConst, Const(_, n), _, _) => ptList.writeConst(n)  // тип лексемы - константа
        }
        token = iterator.nextOption()
        true
    }
  }

  private def statements(): Boolean = {
    if (!statement()) return false
    while (token.isDefined && token.get.tokenName == TokenSep) {
      token = iterator.nextOption()
      if (!statement()) return false
    }
    true
  }

  private def statement(): Boolean = {
    token match {
      case None =>
        error("Ожидается утверждение. Получено ничего")
        false
      case Some(Token(TokenId, Id(_, n), _, _)) =>
        ptList.writeVar(n)  // заносим в ПОЛИЗ переменную
        token = iterator.nextOption()
        token match {
          case None =>
            error("Ожидается знак присваивания. Получено ничего")
            false
          case Some(Token(TokenEq, _, _, _)) =>
            token = iterator.nextOption()
            val fl = arithExpr()
            // ПОЛИЗ для выражения уже сформирован
            ptList.writeCmd(SET)  // заносим в ПОЛИЗ команду присваивания
            fl
          case Some(value) =>
            error("Ожидается знак присваивания. Получено " + value.tokenName + ", pos: " + value.pos)
            false
        }
      case Some(Token(TokenInput, _, _, _)) =>
        token = iterator.nextOption()
        token match {
          case None =>
            error("Ожидается идентификатор. Получено ничего")
            false
          case Some(Token(TokenId, Id(_, n), _, _)) =>
            ptList.writeVar(n)  // заносим в ПОЛИЗ переменную
            ptList.writeCmd(INP)  // заносим в ПОЛИЗ команду ввода
            token = iterator.nextOption()
            true
          case Some(value) =>
            error("Ожидается идентификатор. Получено " + value.tokenName + ", pos: " + value.pos)
            false
        }
      case Some(Token(TokenOutput, _, _, _)) =>
        token = iterator.nextOption()
        val fl = operand()
        // ПОЛИЗ для выражения уже сформирован
        ptList.writeCmd(OUT)  // заносим в ПОЛИЗ команду вывода
        fl
      case Some(value) =>
        error("Ожидается утверждение. Получено " + value.tokenName + ", pos: " + value.pos)
        false
    }
  }

  private def arithExpr(): Boolean = {
    if (!arithExprTerm()) return false
    // ПОЛИЗ для выражения уже сформирован
    while (token.isDefined && token.get.tokenName == TokenArith1) {
      val cmd = token.get.lexeme.c match {
        case "+" => ADD
        case "-" => SUB
      }
      token = iterator.nextOption()
      // ПОЛИЗ для выражения уже сформирован
      if (!arithExprTerm()) return false
      ptList.writeCmd(cmd); //заносим операцию OR в ПОЛИЗ
    }
    true
  }

  private def arithExprTerm(): Boolean = {
    if (!arithExprFactor()) return false
    // ПОЛИЗ для выражения уже сформирован
    while (token.isDefined && token.get.tokenName == TokenArith2) {
      val cmd = token.get.lexeme.c match {
        case "/" => DIV
        case "*" => MUL
      }
      token = iterator.nextOption()
      if (!arithExprFactor()) return false
      // ПОЛИЗ для выражения уже сформирован
      ptList.writeCmd(cmd); //заносим операцию в ПОЛИЗ
    }
    true
  }

  private def arithExprFactor(): Boolean = {
    token match {
      case None => error("Ожидается арифметическое выражение. Получено ничего")
        false
      case Some(Token(TokenOpPar, _, _, _)) =>
        token = iterator.nextOption()
        if (!arithExpr()) return false
        token match {
          case None => error("Ожидается закрывающая скобка. Получено ничего")
            false
          case Some(Token(TokenClPar, _, _, _)) =>
            token = iterator.nextOption()
            true
          case Some(value) => error("Ожидается закрывающая скобка. Получено " + value.tokenName + ", pos: " + value.pos)
            false
        }
      case Some(_) =>
        operand() // ПОЛИЗ для выражения уже сформирован
    }
  }
}

