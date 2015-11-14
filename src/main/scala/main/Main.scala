package main

import interpreter._
import semantics._


/**
 * Created by inzamamrahaman on 14/11/2015.
 */
object Main {

  def main(args : Array[String]) = {

    val sampleProg = Programme(List(Declaration("x", JInt), Declaration("y", JBool)))
    val results = Interpreter.interpret(sampleProg)
    results match {
      case Right(err) => println(err)
      case Left(env) => println(env.mapping.foreach(println))
    }


  }

}
