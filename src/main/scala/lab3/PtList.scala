package lab3

import scala.collection.mutable.ListBuffer

class PtList(val list: ListBuffer[PostfixEntry] = ListBuffer[PostfixEntry]().empty) {
  def size(): Int = list.size
  def writeCmdPtr(ptr: Int): Int = {
    list.addOne(EtCmdPtr(ptr))
    list.size - 1
  }
  def writeCmd(cmd: Cmd): Int = {
    list.addOne(EtCmd(cmd))
    list.size - 1
  }
  def setCmdPtr(ind: Int, ptr: Int): Unit = {
    list(ind) = EtCmdPtr(ptr)
  }
  def writeVar(ind: Int): Int = {
    list.addOne(EtVar(ind))
    list.size - 1
  }
  def writeConst(ind: Int): Int = {
    list.addOne(EtConst(ind))
    list.size - 1
  }
}
