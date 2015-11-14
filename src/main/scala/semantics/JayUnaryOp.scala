package semantics

/**
 * Created by inzamamrahaman on 14/11/2015.
 */

case class JayUnaryOp(op : UnaryOp)


sealed abstract class UnaryOp {

  def eval(val1 : JayValue) : JayValue = val1 match {
    case JayBool(b) => (JayBool(!b))
    case _ => throw new IllegalArgumentException("Error, cannot negate integer!")
  }

}
case object Negate extends UnaryOp