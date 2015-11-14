package semantics

/**
 * Created by inzamamrahaman on 14/11/2015.
 */
abstract sealed class ArithOp {

  def eval(op1 : JayValue, op2 : JayValue) = (op1, op2) match {
    case (JayInt(i1), JayInt(i2)) => this match {
      case Plus => JayInt(i1 + i2)
      case Minus => JayInt(i1 - i2)
      case Mult => JayInt(i1 * i2)
      case Div => JayInt(i1 / i2)
    }
    case _ => throw new IllegalArgumentException("Aithmetic operation misused")
  }

}
case object Plus extends ArithOp
case object Minus extends ArithOp
case object Mult extends ArithOp
case object Div extends ArithOp
