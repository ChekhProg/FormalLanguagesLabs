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
          if (i == length)
            {
              state = F
              add = false
            }
          else {
            val char = str(i)
            if (char.isSpaceChar || char == '\n' || char == '\r') {
              state = S
              add = false
            }
            else if (char.isLetter) state = A
            else if (char.isDigit) state = B
            else if ("+-/*>=();".contains(char)) state = C
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
            else if ("+-/*>=();".contains(char)) state = C
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
            else if ("+-/*>=();".contains(char)) state = C
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
            else if ("+-/*>=();".contains(char)) state = C
            else if (char == '<') state = D
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
        val lexText = str.substring(lexStart, i)
        val tokenName = getTokenName(prevState, lexText)

        val tableId = tokenName match {
          case TokenId =>
            val v = idLexemes.find(_._2 == lexText)
            val tableIdLocal = v match {
              case Some(key -> _) => key
              case None =>
                val endId = idLexemes.size
                idLexemes(idLexemes.size) = lexText
                endId
            }
            tableIdLocal
          case TokenConst =>
            val v = constLexemes.find(_._2 == lexText)
            val tableIdLocal = v match {
              case Some(key -> _) => key
              case None =>
                val endId = constLexemes.size
                constLexemes(constLexemes.size) = lexText
                endId
            }
            tableIdLocal
          case _ => 0
        }

        val newToken = createToken(prevState, lexText, lexInd, lexStart, tableId)
        tokens += newToken

        lexInd += 1
        lexStart = i //
      }

      if (state != prevState) lexStart = i
      if ((state != E) && (state != F)) i += 1
    }
    if (state == E) throw new Exception("Lexeme Analyzer Exception: " +  str(i) + ", " + lexInd + ", " + lexStart)
    LexData(tokens.toList, idLexemes.toMap, constLexemes.toMap)
  }

  def getTokenName(state: State, lexText: String): TokenName = {
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
      case C if lexText == "+" | lexText == "-" => TokenArith1
      case C if lexText == "*" | lexText == "/" => TokenArith2
      case C if lexText == "=" => TokenEq
      case C if lexText == ";" => TokenSep
      case C if lexText == "(" => TokenOpPar
      case C if lexText == ")" => TokenClPar
    }
    tokenName
  }

  def createToken(state: State, lexText: String, lexInd: Int, lexPos: Int, tableId: Int): Token = {
    val tokenName = getTokenName(state, lexText)
    val token = (state, tokenName) match {
      case (A, TokenId) => Token(tokenName, Id(lexText, tableId), lexInd, lexPos)
      case (B, _) => Token(tokenName, Const(lexText, tableId), lexInd, lexPos)
      case (_, _) => Token(tokenName, Lex(lexText), lexInd, lexPos)

    }
    token
  }
}
