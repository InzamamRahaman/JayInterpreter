package semantics

/**
 * Created by inzamamrahaman on 14/11/2015.
 */
abstract sealed class JayBinOp
case class JayBinArithOp(op : ArithOp)
case class JayBinRelOp(op : RelOp)
case class JayBinBoolOp(op : BinBoolOp)
