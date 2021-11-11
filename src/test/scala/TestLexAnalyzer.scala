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
    val source = scala.io.Source.fromFile("src/test/resources/lextest.txt")
    val str = try source.mkString finally source.close()
    val data = LexData.lexAnalysis(str)
    assertEquals(Lex("<"), data.tokens(3).lexeme)
    assertEquals(Id("max", 0), data.tokens(5).lexeme)
    assertEquals("max", data.idTable(0))
  }
}
