package semantics

/**
 * Created by inzamamrahaman on 14/11/2015.
 */

sealed abstract class BinBoolOp {

  def eval(op1 : JayValue, op2 : JayValue) = (op1, op2) match {
    case ((JayBool(b1), JayBool(b2))) => this match {
      case AND => JayBool(b1 && b2)
      case OR => JayBool(b1 || b2)
    }
    case _ => throw new IllegalArgumentException("Incorrect application of bianry boolean operation")
  }

}
case object AND extends BinBoolOp
case object OR extends BinBoolOp
