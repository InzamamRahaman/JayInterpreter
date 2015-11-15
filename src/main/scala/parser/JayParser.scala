package parser


/**
 * Created by inzamamrahaman on 14/11/2015.
 */

import org.parboiled2._
import semantics._

class JayParser(val input : ParserInput) extends Parser {


  def InputLine : Rule1[Node] = rule {
    ParseProgramme ~ EOI ~> ((xs : Node) => xs)
  }


  // Importants constant parsers
  def wps : Rule0 = rule(zeroOrMore(' '))
  def nls : Rule0 = rule(zeroOrMore('\n'))
  def blnk : Rule0 = rule(zeroOrMore(anyOf(" \n")))//rule(wps | nls)

  implicit def whps(s : String) : Rule0 = rule {
    str(s) ~ blnk
  }
  
//  def ParseProgramme : Rule1[Node] = rule {
//    blnk ~ Void ~ ParseMain ~ lcb ~ ParseMultipleDeclaration ~ blnk ~ rcb ~>
//      ((xs : Seq[Node]) => 
//        Programme(xs.toList))
//  }
  
  def ParseProgramme : Rule1[Node] = rule {
    blnk ~ Void ~ ParseMain ~ lcb ~ ParseMultipleDeclaration ~ blnk ~
    ParseMultipleStatements ~ blnk ~ rcb ~>
      ((xs : Seq[Node], ys : Seq[Node]) => 
        Programme(xs.toList ++ ys.toList))
  }

  def Void : Rule0 = "void"
  def ParseMain : Rule0 = "main()"
  def lpa : Rule0 = "("
  def rpa : Rule0 = ")"
  def lcb : Rule0 = "{"
  def rcb : Rule0 = "}"
  def Else : Rule0 = "else"
  def If : Rule0 = "if"
  def While : Rule0 = "while"
  def LEnd : Rule0 = ";"
  def IntKey : Rule0 = "int"
  def BoolKey : Rule0 = "bool"
  def Comma : Rule0 = ","
  def PlusSign : Rule0 = "+"
  def MinusSign : Rule0 = "-"
  def MultSign : Rule0 = "*"
  def DivSign : Rule0 = "/"
  def LTSign : Rule0 = "<"
  def GTSign : Rule0 = ">"
  def LTESign : Rule0 = "<="
  def GTESign : Rule0 = ">="
  def EQSign : Rule0 = "=="
  def NEQSign : Rule0 = "!="
  def AndSign : Rule0 = "&&"
  def OrSign : Rule0 = "||"
  def NegSign : Rule0 = "!"
  def Equal : Rule0 = "="
  
  def ParseBinOp : Rule1[String] = rule {
    capture(PlusSign | MinusSign | MultSign | DivSign | 
    LTSign | LTESign | GTSign | GTESign | EQSign |
    NEQSign | AndSign | OrSign) ~> 
    ((xs : String) => xs.filter { x => x != ' ' })
  }



  def Ident : Rule1[String] = rule {
    capture(oneOrMore(CharPredicate.Alpha)) ~ capture(zeroOrMore(CharPredicate.AlphaNum)) ~>
      ((s : String, t : String) => s + t)
  }


  // Rules specific to parsing valid Jay code

  // rules for parsing declarations

  def ParseBoolDeclaration : Rule1[Node] = rule {
    BoolKey ~ Ident ~ LEnd ~> ((x : String) => Declaration(x, JBool))
  }

  def ParseIntDeclaration : Rule1[Node] = rule {
    IntKey ~ Ident ~ LEnd ~> ((x : String) => Declaration(x, JInt))
  }

  def ParseDeclaration : Rule1[Node] = rule {
    (ParseBoolDeclaration | ParseIntDeclaration)
  }
  
  def ParseMultipleDeclaration : Rule1[Seq[Node]] = 
    rule(zeroOrMore(ParseDeclaration).separatedBy(blnk))



  // Rules for parsing expressions
  def True : Rule1[JayValue] = rule {
    (whps("true")) ~> (() => JayBool(true))
  }

  def False : Rule1[JayValue] = rule {
    (whps("false")) ~> (() => JayBool(false))
  }

  def convertToInt(sign : Option[String], body : String) = sign match {
    case None => body.toInt
    case Some(_) => -1 * body.toInt
  }

  def ParseNegativeInt : Rule1[JayValue] = rule {
    ('-' ~ capture(CharPredicate.Digit)) ~> ((x : String) => JayInt(x.toInt * -1))
  }

  def ParseNonNegativeInt : Rule1[JayValue] = rule {
    capture(CharPredicate.Digit) ~> ((x : String) => JayInt(x.toInt))
  }

  def ParseValues : Rule1[JayExpression] = rule {
    (True | False | ParseNegativeInt | ParseNonNegativeInt) ~> Value
  }

  def ParseVariable : Rule1[JayExpression] = rule {
    Ident ~> Variable
  }

  def ParseJayExpression : Rule1[JayExpression] = rule {
    (ParseBinExpression | ParseValues | ParseVariable | ParseUnaryOperation)
  }

  def ParseUnaryOperation : Rule1[JayExpression] = rule {
    NegSign ~ ParseJayExpression ~> ((x : JayExpression) => UnaryExpression(JayUnaryOp(Negate),x))
  }

  def ParseBinExpression : Rule1[JayExpression] = rule {
    ParseJayExpression ~ blnk ~ ParseBinOp ~ blnk ~ ParseJayExpression ~> 
      ((exp1 : JayExpression, op : String, exp2 : JayExpression) => 
        BinExpression(stringToBinOp(op), exp1, exp2))
  }
  
  // rules for parsing statements
  
  def ParseMultipleStatements : Rule1[Seq[Node]] = rule {
    zeroOrMore(ParseStatement).separatedBy(blnk) ~>
    ((stmnts : Seq[Node]) => stmnts)
  }
  
  def ParseStatement : Rule1[Node] = rule {
    ParseJayStatement ~> Statement
  }
  
  def ParseJayStatement : Rule1[JayStatement] = rule {
    (ParseAssignment | ParseConditional | ParseLoop)
  }
  
  def ParseAssignment : Rule1[JayStatement] = rule {
    Ident ~ blnk ~ Equal ~ ParseJayExpression ~ LEnd ~> 
      ((name : String, exp : JayExpression) => Assignment(name, exp))
  }
  
  def ParseConditional : Rule1[JayStatement] = rule {
    If ~ lpa ~ ParseJayExpression ~ blnk ~ rpa ~ lcb ~
      ParseJayStatement ~ blnk ~ rcb ~ Else ~ lcb ~ ParseJayStatement ~
      blnk ~ rcb ~> 
    ((exp : JayExpression, st1 : JayStatement, st2: JayStatement) =>
      Conditional(exp, st1, st2))
  }
  
  def ParseLoop : Rule1[JayStatement] = rule {
    While ~ lpa ~ ParseJayExpression ~ blnk ~ rpa ~ lcb ~
    ParseJayStatement ~ blnk ~ rcb ~> 
    ((exp : JayExpression, st : JayStatement) => Loop(exp, st))
  }
  
  
  def ParseBlock : Rule1[JayStatement] = rule {
    zeroOrMore(ParseJayStatement).separatedBy(blnk) ~>
      ((xs : Seq[JayStatement]) => Block(xs))
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

//  def ParseBinOp : Rule1[JayBinOp] = rule {
//    capture("+" | "-" | "/" | "*"| ">" | "<" | "&&" | "||" | ">=" | "<=" | "==" | "!=") ~>
//      (str => stringToBinOp(str))
//  }



  def stringToUnaryOp(str : String) = str match {
    case "!" => JayUnaryOp(Negate)
    case _ => throw new IllegalArgumentException("No such valid unary operator")
  }



}
