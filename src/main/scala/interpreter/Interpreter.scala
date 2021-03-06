package interpreter

import semantics._

/**
 * Created by inzamamrahaman on 14/11/2015.
 */
object Interpreter {

  private val defaultState : State = Left(new Environment(Map()))

  type State = Either[Environment, String]


  private def handleState(node : Node, state : State) : State = state match {
    case Right(error) => Right(error)
    case Left(env) => interpretNode(node, env)
  }

  private def interpretNode(node : Node, env : Environment) : State = node match {
    case Declaration(name, typeOf) => Left(env.declare(name, typeOf))
    case Statement(statement) => statement.eval(Left(env))
    case _ => Right("full functionality not yet implemented")
  }

//  case Statement(Assignment(name, exp)) => exp.internalEval(env) match {
//      case Left((res, env2)) => env2.set(name, res)
//      case Right(err) => Right(err)
//    }


  def interpret(prog : Node) : State = prog match {
    case Programme(nodes) =>
      nodes.foldLeft(defaultState)((state : State, n : Node) => handleState(n, state))
    case _ => throw new IllegalArgumentException("Error in code! interpet is supposed to be used with entire programme")
  }



}
