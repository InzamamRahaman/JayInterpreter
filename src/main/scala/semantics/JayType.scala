package semantics

/**
 * Created by inzamamrahaman on 14/11/2015.
 */

sealed abstract class JayType {
  override def toString = this match {
    case JInt => "Integer"
    case JBool => "Boolean"
  }
}
case object JInt extends JayType
case object JBool extends JayType
