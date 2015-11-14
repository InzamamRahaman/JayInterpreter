package semantics

/**
 * Created by inzamamrahaman on 14/11/2015.
 */

sealed abstract class JayValue {

  override def toString = this match {
    case Undef => "undef"
    case JayInt(i) => i.toString
    case JayBool(b) => b.toString
  }

  def getCorrectType = this match {
    case Undef => throw new IllegalArgumentException("Undef has no type! Does not need type checking")
    case JayInt(_) => JInt
    case JayBool(_) => JBool
  }

  def getCorrectTypeName = this.getCorrectType.toString

}
case object Undef extends JayValue
case class JayInt(value : Int) extends JayValue
case class JayBool(value : Boolean) extends JayValue
