package lab1

trait Lexeme {
  def c: String
}
case class Lex(c: String) extends Lexeme
case class Id(c: String, n: Int) extends Lexeme
case class Const(c: String, n: Int) extends Lexeme
