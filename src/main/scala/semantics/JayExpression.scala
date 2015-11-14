package semantics

/**
 * Created by inzamamrahaman on 14/11/2015.
 */

import interpreter.Interpreter.State
import scala.annotation.tailrec


sealed abstract class JayExpression {


  private def binOperationMatch(op: JayBinOp, val1: JayValue, val2: JayValue): Either[JayValue, String] =
    (op, val1.getCorrectType, val2.getCorrectType) match {
      case (JayBinArithOp(op1), JInt, JInt) => Left(op1.eval(val1, val2))
      case ((JayBinRelOp(op1), JInt, JInt)) => Left(op1.eval(val1, val2))
      case ((JayBinBoolOp(op1), JBool, JBool)) => Left(op1.eval(val1, val2))
      case _ => Right("Error, incompatiable types and operation")
    }

  private def unaryOperationMatch(op : JayUnaryOp, val1 : JayValue) : Either[JayValue, String] = val1 match {
    case JayBool(b) => op match {
      case JayUnaryOp(op1) => Left(op1.eval(val1))
    }
    case _ => Right("Error, incompatible types and operation")
  }


  def internalEval(env : Environment) : Either[(JayValue, Environment), String] = this match {
    case Value(exp) => Left((exp, env))
    case Variable(name) => env.retrieveValue(name) match {
      case None => Right("No variable of name " + name + " exists! Must be declared before use")
      case Some(v) => Left((v, env))
    }
    case BinExpression(op, exp1, exp2) => (exp1.internalEval(env), exp2.internalEval(env)) match {
      case (Left(p1), Left(p2)) => binOperationMatch(op, p1._1, p2._1) match {
        case Left(res) => Left(res, env)
        case Right(err) => Right(err)
      }
      case ((Right(err1), Right(err2))) => Right(err1 + ". " + err2)
      case (_, Right(err2)) => Right(err2)
      case (Right(err1), _) => Right(err1)
    }
    case UnaryExpression(op, exp1) => (exp1.internalEval(env)) match {
      case Left(p1) => unaryOperationMatch(op, p1._1) match {
        case Left(res) => Left(res, env)
        case Right(err) => Right(err)
      }
      case Right(err) => Right(err)
    }

  }

}
case class BinExpression(op : JayBinOp, exp1 : JayExpression, exp2 : JayExpression) extends JayExpression
case class UnaryExpression(op : JayUnaryOp, exp1 : JayExpression) extends JayExpression
case class Value(exp1 : JayValue) extends JayExpression
//case class Assignment(destination : String, source : JayExpression)
case class Variable(exp1 : String) extends JayExpression

//case Assignment(dest, source) => source.internalEval(env) match {
//case Right(err) => Right(err)
//case Left((valid, env2)) => env2.set(dest, valid) match {
//case Left(env3) => Left((valid, env3))
//case Right(err2) => Right(err2)
//}
