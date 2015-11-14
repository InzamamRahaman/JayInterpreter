package parser


/**
 * Created by inzamamrahaman on 14/11/2015.
 */

import org.parboiled2._
import semantics._

class JayParser(val input : ParserInput) extends Parser {


  def InputLine : Rule1[Node] = rule {
    ParseDeclaration ~ EOI ~> ((xs : Seq[Node]) => Programme(xs))
  }


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

  def ParseIdentInExpression : Rule1[JayExpression] = rule {
    ParseIdentifier ~> (x => Variable(x))
  }

  def ParseTrue : Rule1[JayValue] = rule {
    "true" ~> (() => JayBool(true))
  }

  def ParseFalse : Rule1[JayValue] = rule {
    "false" ~> (() => JayBool(false))
  }

  def ParseInteger : Rule1[JayValue] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((x : String) => JayInt(x.toInt))
  }

  def ParseValue : Rule1[JayExpression] = rule {
    (ParseTrue | ParseFalse | ParseInteger) ~> ((x : JayValue) => Value(x))
  }

  def ParseExpression : Rule1[Node] = rule {
    ParseJayExpression ~> (x => Expression(x))
  }

  def ParseBlock : Rule1[JayStatement] = rule {
    zeroOrMore(ParseJayStatement) ~> ((stmnts : Seq[JayStatement]) => Block(stmnts))
  }


  def ParseMultipleStatements : Rule1[Seq[Node]] = rule {
    zeroOrMore(ParseStatement).separatedBy(ParseMaybeWhiteSpace)
  }

  def ParseStatement : Rule1[Node] = rule {
    ParseJayStatement ~> ((x : JayStatement) => Statement(x))
  }


  def ParseJayStatement : Rule1[JayStatement] = rule {
    (ParseAssignment | ParseBlock)
  }

  def ParseConditionalStatement : Rule1[JayStatement] = rule {
    ("if" ~ ParseMaybeWhiteSpace ~ "(" ~ ParseMaybeWhiteSpace
      ~ ParseJayExpression ~ ")" ~ ParseMaybeWhiteSpace ~ "{" ~
      ParseMaybeWhiteSpace  )
  }

  def ParseAssignment : Rule1[JayStatement] = rule {
    (ParseIdentifier ~ ParseMaybeWhiteSpace ~ "=" ~ ParseMaybeWhiteSpace ~
      ParseJayExpression ~ ParseLineTerminator) ~>
      ((ident : String, exp : JayExpression) => Assignment(ident, exp))
  }

  def ParseJayExpression : Rule1[JayExpression] = rule {
    (ParseIdentInExpression | ParseValue | ParseBinaryOperation)
  }

  def stringToBinOp(str : String) : JayBinOp = str match {
    case "+" => JayBinArithOp(Plus)
    case "-" => JayBinArithOp(Minus)
    case "*" => JayBinArithOp(Mult)
    case "/" => JayBinArithOp(Div)
    case "&&" => JayBinBoolOp(AND)
    case "||" => JayBinBoolOp(OR)
    case "==" => JayBinRelOp(EQ)
    case "!=" => JayBinRelOp(NEQ)
    case ">" => JayBinRelOp(GT)
    case ">=" => JayBinRelOp(GTE)
    case "<" => JayBinRelOp(LT)
    case "<=" => JayBinRelOp(LTE)
    case _ => throw new IllegalArgumentException("Not a known binary operation")
  }

  def ParseBinOp : Rule1[JayBinOp] = rule {
    capture("+" | "-" | "/" | "*"| ">" | "<" | "&&" | "||" | ">=" | "<=" | "==" | "!=") ~>
      (str => stringToBinOp(str))
  }

  def ParseBinaryOperation : Rule1[JayExpression] = rule {
    (ParseJayExpression ~ ParseBinOp ~ ParseJayExpression) ~>
      ((x : JayExpression, y : JayBinOp, z : JayExpression) => BinExpression(y, x, z))
  }

  def stringToUnaryOp(str : String) = str match {
    case "!" => JayUnaryOp(Negate)
    case _ => throw new IllegalArgumentException("No such valid unary operator")
  }

  def ParseUnaryOp : Rule1[JayUnaryOp] = rule {
    capture("!") ~> (str => stringToUnaryOp(str))
  }

  def ParseUnaryOperation : Rule1[JayExpression] = rule {
    (ParseUnaryOp ~ ParseJayExpression) ~>
      ((a : JayUnaryOp, b : JayExpression) => UnaryExpression(a, b))
  }

//  def ParseMultipleDeclarations: Rule1[Seq[Node]] = rule {
//    zeroOrMore(ParseDeclaration) ~> ((x : Seq[Seq[Node]]) => x.flatten.toList)
//  }

}
