package semantics

/**
 * Created by inzamamrahaman on 14/11/2015.
 */
abstract sealed class JayBinOp
case class JayBinArithOp(op : ArithOp) extends JayBinOp
case class JayBinRelOp(op : RelOp) extends JayBinOp
case class JayBinBoolOp(op : BinBoolOp) extends JayBinOp
