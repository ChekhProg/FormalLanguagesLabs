import junit.framework.TestCase
import lab1._
import org.junit.Assert._

class TestLexAnalyzer extends TestCase{
  def testStringOne(): Unit ={
    val data = LexData.lexAnalysis("do while 100<111 max = (66 / 33)\nloop")
    assertEquals(TokenConst, data.tokens(2).tokenName)
    assertEquals(Const("100",0), data.tokens(2).lexeme)
    assertEquals("100", data.constTable(0))
  }

  def testTxtOne(): Unit = {
    val data = LexData.lexAnalysis("do while 1<0 or 2<>3\n    m = 3 / 4;\n    s = (3 + i) + (1 /2);\n    output m\nloop")
    assertEquals(Lex(";"), data.tokens(14).lexeme)
    assertEquals(Lex("<>"), data.tokens(7).lexeme)
  }
}
