package lab3

trait Cmd
case object JMP extends Cmd
case object JZ extends Cmd
case object SET extends Cmd
case object ADD extends Cmd
case object SUB extends Cmd
case object AND extends Cmd
case object OR extends Cmd
case object CMPE extends Cmd
case object CMPNE extends Cmd
case object CMPL extends Cmd
case object CMPG extends Cmd
case object INP extends Cmd
case object OUT extends Cmd
case object MUL extends Cmd
case object DIV extends Cmd
