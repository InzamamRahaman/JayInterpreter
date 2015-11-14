package semantics

/**
 * Created by inzamamrahaman on 14/11/2015.
 */
abstract sealed class RelOp {

  def eval(op1 : JayValue, op2 : JayValue) = (op1, op2) match {
    case (JayInt(i1), JayInt(i2)) => this match {
      case LT => JayBool(i1 < i2)
      case LTE => JayBool(i1 <= i2)
      case EQ => JayBool(i1 == i2)
      case NEQ => JayBool(i1 != i2)
      case GT => JayBool(i1 > i2)
      case GTE => JayBool(i1 >= i2)
    }
    case _ => throw new IllegalArgumentException("Misuse or relational operator")
  }

}
case object LT extends RelOp
case object LTE extends RelOp
case object EQ extends RelOp
case object NEQ extends RelOp
case object GT extends RelOp
case object GTE extends RelOp

