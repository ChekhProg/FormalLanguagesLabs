package lab1

import lab1.State._


case class LexData(tokens: List[Token], idTable: Map[Int, String], constTable: Map[Int, String]){
  def printLexData(): Unit = {
    print("Tokens: ")
    println(tokens)
    print("Id Table: ")
    println(idTable)
    print("Const Table: ")
    println(constTable)
  }
}

object LexData {
  def lexAnalysis(str: String): LexData = {
    var state = S
    var prevState = S
    var add = false
    var i = 0
    var lexInd = 0
    var lexStart = 0
    val length = str.length

    val tokens = scala.collection.mutable.ListBuffer[Token]().empty
    val idLexemes = scala.collection.mutable.Map[Int, String]().empty
    val constLexemes = scala.collection.mutable.Map[Int, String]().empty

    while ((state != E) && (state != F)) {
      prevState = state
      add = true
      state match {
        case S =>
          if (i == length) state = F
          else {
            val char = str(i)
            if (char.isSpaceChar || char == '\n' || char == '\r') state = S
            else if (char.isLetter) state = A
            else if (char.isDigit) state = B
            else if ("+-/*>=()".contains(char)) state = C
            else if (char == '<') state = D
            else state = E
            add = false
          }
        case A =>
          if (i == length) state = F
          else {
            val char = str(i)
            if (char.isSpaceChar || char == '\n' || char == '\r') state = S
            else if (char.isLetterOrDigit) add = false
            else if ("+-/*>=()".contains(char)) state = C
            else if (char == '<') state = D
            else {
              state = E
              add = false
            }
          }
        case B =>
          if (i == length) state = F
          else {
            val char = str(i)
            if (char.isSpaceChar || char == '\n' || char == '\r') state = S
            else if (char.isDigit) {
              state = B
              add = false
            }
            else if ("+-/*>=()".contains(char)) state = C
            else if (char == '<') state = D
            else {
              state = E
              add = false
            }
          }
        case C =>
          if (i == length) state = F
          else {
            val char = str(i)
            if (char.isSpaceChar || char == '\n' || char == '\r') state = S
            else if (char.isLetter) state = A
            else if (char.isDigit) state = B
            else {
              state = E
              add = false
            }
          }
        case D =>
          if (i == length) state = F
          else {
            val char = str(i)
            if (char.isSpaceChar || char == '\n' || char == '\r') state = S
            else if (char.isLetter) state = A
            else if (char.isDigit) state = B
            else if (char == '>') {
              state = D
              add = false
            }
            else {
              state = E
              add = false
            }
          }
      }
      if (add) {
        val newToken = createToken(prevState, str.substring(lexStart, i), lexInd, lexStart,
          idLexemes.size,
          constLexemes.size)
        tokens += newToken
        if (newToken.tokenName == TokenId) {
          val lexeme = newToken.lexeme.asInstanceOf[Id]
          val id = lexeme.n
          idLexemes(id) = lexeme.c
        }
        if (newToken.tokenName == TokenConst) {
          val lexeme = newToken.lexeme.asInstanceOf[Const]
          val id = lexeme.n
          constLexemes(id) = lexeme.c
        }
        lexInd += 1
      }

      if (state != prevState) lexStart = i
      if ((state != E) && (state != F)) i += 1
    }
    if (state == E) throw new Exception("Lexeme Analyzer Exception")
    LexData(tokens.toList, idLexemes.toMap, constLexemes.toMap)
  }

  def createToken(state: State, lexText: String, lexInd: Int, lexPos: Int, idsLength: Int, constLength: Int): Token = {
    val tokenName = state match {
      case A if lexText == "do" => TokenDo
      case A if lexText == "while" => TokenWhile
      case A if lexText == "loop" => TokenLoop
      case A if lexText == "and" => TokenAnd
      case A if lexText == "or" => TokenOr
      case A if lexText == "input" => TokenInput
      case A if lexText == "output" => TokenOutput
      case A => TokenId
      case B => TokenConst
      case C if lexText == ">" | lexText == "<" => TokenRel
      case D if lexText == "<>" => TokenRel
      case D if lexText == "<" => TokenRel
      case C if lexText == "+" | lexText == "-" | lexText == "*" | lexText == "/" => TokenArith
      case C if lexText == "=" => TokenEq
      case C if lexText == ";" => TokenSep
      case C if lexText == "(" => TokenOpPar
      case C if lexText == ")" => TokenClPar
    }
    val token = (state, tokenName) match {
      case (A, TokenId) => Token(tokenName, Id(lexText, idsLength), lexInd, lexPos)
      case (B, _) => Token(tokenName, Const(lexText, constLength), lexInd, lexPos)
      case (_, _) => Token(tokenName, Lex(lexText), lexInd, lexPos)

    }
    token
  }
}
