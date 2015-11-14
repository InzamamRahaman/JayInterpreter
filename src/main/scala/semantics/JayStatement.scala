package semantics

/**
 * Created by inzamamrahaman on 14/11/2015.
 */

import interpreter.Interpreter.State

sealed abstract class JayStatement {

  def eval(state : State) : State = state match {
    case Right(err) => state
    case Left(env) => this match {
      case Block(body) => body.foldLeft(state)((acc : State, stmnt : JayStatement) => stmnt.eval(acc))
      case Assignment(target, exp) => exp.internalEval(env) match {
        case Left((value, env1)) => env1.set(target, value)
        case Right(err) => Right(err)
      }
      case Conditional(test, exp1, exp2) => test.internalEval(env) match {
        case Left((value, env1)) => value match {
          case (JayBool(false) | JayInt(0)) => exp1.internalEval(env1) match {
            case Left((_, env2)) => Left(env2)
            case Right(err) => Right(err)
          }
          case _ => exp1.internalEval(env1) match {
            case Left((_, env2)) => Left(env2)
            case Right(err) => Right(err)
          }
        }
        case Right(err) => Right(err)
      }
      case loop @ Loop(test, body) => test.internalEval(env) match {
        case Right(err) => Right(err)
        case Left((value, env1)) => value match {
          case (JayBool(false) | JayInt(0)) => Left(env1)
          case _ => body.internalEval(env1) match {
            case Right(err) => Right(err)
            case Left((value2, env2)) => loop.eval(Left(env2))
          }
        }
      }
    }
  }

}
case class Assignment(target : String, exp : JayExpression) extends JayStatement
case class Conditional(test : JayExpression, thenpart : JayStatement, elsepart : JayStatement) extends JayStatement
case class Loop(test : JayExpression, body : JayStatement) extends JayStatement
case class Block(body : List[JayStatement]) extends JayStatement


