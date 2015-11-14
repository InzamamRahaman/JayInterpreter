package parser


/**
 * Created by inzamamrahaman on 14/11/2015.
 */

import org.parboiled2._
import semantics._

class JayParser(val input : ParserInput) extends Parser {


  def InputLine = rule { "Hi" ~ EOI }


  def ParseLineTerminator : Rule0 = rule {
    ";"
  }

  def ParseMaybeWhiteSpace : Rule0 = rule {
    zeroOrMore(" " | "\n")
  }

  def ParseJayTypeIntPred = rule { "int" }
  def ParseJayTypeInt : Rule1[JayType] = rule {
    ParseJayTypeIntPred ~> (() => JInt)
  }

  def ParseJayTypeBoolPred = rule { "bool" }
  def ParseJayTypeBool : Rule1[JayType] = rule {
    ParseJayTypeIntPred ~> (() => JBool)
  }

  def ParseTypeDeclaration : Rule1[JayType] = rule {
    ParseJayTypeInt | ParseJayTypeBool
  }

  def ParseIdentifier : Rule1[String] = rule {
    capture(oneOrMore(CharPredicate.Alpha) ~ oneOrMore(CharPredicate.AlphaNum))
  }

  def ParseMultipleIdentifiers : Rule1[Seq[String]] = rule {
    oneOrMore(ParseIdentifier ~ ParseMaybeWhiteSpace).separatedBy(",") ~ ParseLineTerminator
  }

  def ParseDeclaration : Rule1[Seq[Node]] = rule {
    ParseTypeDeclaration ~ oneOrMore(" " | "\n") ~ ParseMultipleIdentifiers ~>
      ((td : JayType, sSeq : Seq[String]) => sSeq.map(s => Declaration(s, td)))
  }

  def ParseMultipleDeclarations: Rule1[Seq[Node]] = rule {
    zeroOrMore(ParseDeclaration) ~> ((x : Seq[Seq[Node]]) => x.flatten.toList)
  }

}
