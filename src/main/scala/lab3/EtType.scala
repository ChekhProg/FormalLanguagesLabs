package lab3

trait PostfixEntry
trait EtType extends PostfixEntry { def n : Int }
case class EtVar(n: Int) extends EtType
case class EtConst(n: Int) extends EtType
case class EtCmdPtr(n: Int) extends EtType
sealed trait EtCmd extends PostfixEntry
case object JMP extends EtCmd
case object JZ extends EtCmd
case object SET extends EtCmd
case object ADD extends EtCmd
case object SUB extends EtCmd
case object AND extends EtCmd
case object OR extends EtCmd
case object CMPE extends EtCmd
case object CMPNE extends EtCmd
case object CMPL extends EtCmd
case object CMPG extends EtCmd
case object INP extends EtCmd
case object OUT extends EtCmd
case object MUL extends EtCmd
case object DIV extends EtCmd