package main

import interpreter.Interpreter
import semantics._
import parser._
import scala.util.{Try, Success, Failure}


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
    
    val sampleFactorial = 
      """
      void main() {
        int n;
        int fib0;
        int fib1;
        int temp;
        int result;
        n = 8;
        fib0 = 0;
        fib1 = 1;
        while (n > 0) {
          
        }
        result = fib0;
      }  
      """
    
    val sampleCode = 
      """
      void main() {
        int n;
        int m;
        n = 1;
        m = 2;
        m = m + 2;
        if (true) {
          n = n * m;
        } else {
          n = n;
        }
      }
      """
    
    
    val parserResults = new JayParser(sampleFactorial).InputLine.run()
    println(parserResults)
    parserResults match {
      case Success(tree) => {
        val state = Interpreter.interpret(tree)
        state match {
          case Left(env) => env.mapping.foreach(println)
          case Right(err) => println(err)
        }
      }
      case Failure(err) => println(err)
    }
    
    val str = "x = x + 2;"
    val res1 = new JayParser(str).ParseAssignment.run()
    println(res1)

  }

}
