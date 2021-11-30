package lab3

trait PostfixEntry
trait EtType extends PostfixEntry { def n : Int }
case class EtVal(n: Int) extends EtType
case class EtVar(n: Int) extends EtType
case class EtConst(n: Int) extends EtType
case class EtCmdPtr(n: Int) extends EtType
case class EtCmd(et: Cmd) extends PostfixEntry
