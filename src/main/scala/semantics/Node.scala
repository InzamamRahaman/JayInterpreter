package semantics

/**
 * Created by inzamamrahaman on 14/11/2015.
 */

abstract sealed class Node
case class Programme(contents : Seq[Node]) extends Node
case class Declaration(name : String, typeOf : JayType) extends Node
case class Expression(exp : JayExpression) extends Node
















