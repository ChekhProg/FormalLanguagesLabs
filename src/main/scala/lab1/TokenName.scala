package lab1

sealed trait TokenName
case object TokenDo extends TokenName
case object TokenWhile extends TokenName
case object TokenLoop extends TokenName
case object TokenAnd extends TokenName
case object TokenOr extends TokenName
case object TokenInput extends TokenName
case object TokenOutput extends TokenName
case object TokenRel extends TokenName
case object TokenArith extends TokenName
case object TokenSep extends TokenName
case object TokenEq extends TokenName
case object TokenId extends TokenName
case object TokenConst extends TokenName
case object TokenOpPar extends TokenName
case object TokenClPar extends TokenName