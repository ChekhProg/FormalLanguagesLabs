package lab2

import lab1._

class SyntaxAnalyzer(lexData: LexData, var iterator: Iterator[Token] = null, var token: Option[Token] = None) {
  def syntaxAnalysis(): Boolean = {
    iterator = lexData.tokens.iterator
    token = iterator.nextOption()
    whileStatement()
  }

  private def error(msg: String): Unit = {
    println(msg)
  }

  private def whileStatement(): Boolean = {
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
        error("Ожидается 'while'. Получено " + value.tokenName  + ", pos: " + value.pos)
        return false
      case Some(_) => ()
    }
    token = iterator.nextOption()
    if (!condition()) return false
    if (!statements()) return false
    token match {
      case None =>
        error("Ожидается 'loop'. Получено ничего")
        return false
      case Some(value) if value.tokenName != TokenLoop =>
        error("Ожидается 'loop'. Получено " + value.tokenName  + ", pos: " + value.pos)
        return false
      case Some(_) => ()
    }
    true
  }

  private def condition(): Boolean = {
    if(!logExpr()) return false
    while(token.isDefined && token.get.tokenName == TokenOr) {
      token = iterator.nextOption()
      if (! logExpr()) return false
    }
    true
  }

  private def logExpr(): Boolean = {
    if (!relExpr()) return false
    while (token.isDefined && token.get.tokenName == TokenAnd) {
      token = iterator.nextOption()
      if (!relExpr()) return false
    }
    true
  }

  private def relExpr(): Boolean = {
    if (!operand()) return false
    if (token.isDefined && (token.get.tokenName == TokenRel || token.get.tokenName == TokenEq)) {
      token = iterator.nextOption()
      if (!operand()) return false
    }
    true
  }

  private def operand(): Boolean = {
    token match {
      case None =>
        error("Ожидается переменная или константа. Получено ничего")
        false
      case Some(value) if value.tokenName != TokenId && value.tokenName != TokenConst =>
        error("Ожидается переменная или константа. Получено " + value.tokenName  + ", pos: " + value.pos)
        false
      case Some(_) => token = iterator.nextOption()
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
      case Some(Token(TokenId, _, _, _)) =>
        token = iterator.nextOption()
        token match {
          case None =>
            error("Ожидается знак присваивания. Получено ничего")
            false
          case Some(Token(TokenEq, _, _, _)) =>
            token = iterator.nextOption()
            arithExpr()
          case Some(value) =>
            error("Ожидается знак присваивания. Получено " + value.tokenName  + ", pos: " + value.pos)
            false
        }
      case Some(Token(TokenInput, _, _, _)) =>
        token = iterator.nextOption()
        token match {
          case None =>
            error("Ожидается идентификатор. Получено ничего")
            false
          case Some(Token(TokenId, _, _, _)) =>
            token = iterator.nextOption()
            true
          case Some(value) =>
            error("Ожидается идентификатор. Получено " + value.tokenName  + ", pos: " + value.pos)
            false
        }
      case Some(Token(TokenOutput, _, _, _)) =>
        token = iterator.nextOption()
        operand()
      case Some(value) =>
        error("Ожидается утверждение. Получено " + value.tokenName  + ", pos: " + value.pos)
        false
    }
  }

  private def arithExpr(): Boolean = {
    if (!arithExprTerm()) return false
    while (token.isDefined && token.get.tokenName == TokenArith1) {
      token = iterator.nextOption()
      if (!arithExprTerm()) return false
    }
    true
  }

  private def arithExprTerm(): Boolean = {
    if (!arithExprFactor()) return false
    while (token.isDefined && token.get.tokenName == TokenArith2) {
      token = iterator.nextOption()
      if (!arithExprFactor()) return false
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
          case Some(value) => error("Ожидается закрывающая скобка. Получено " + value.tokenName  + ", pos: " + value.pos)
            false
        }
      case Some(_) => operand()
    }
  }
}
