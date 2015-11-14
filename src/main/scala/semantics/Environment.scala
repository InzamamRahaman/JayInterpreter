package semantics

/**
 * Created by inzamamrahaman on 14/11/2015.
 */


class Environment(val mapping : Map[String, (JayType, JayValue)]) {

  def retrieve(name : String) : Option[(JayType, JayValue)] =
    mapping.get(name)

  def retrieveType(name : String) : Option[JayType] = retrieve(name) match {
    case None => None
    case Some((t, v)) => Some(t)
  }

  def retrieveValue(name : String) : Option[JayValue] = retrieve(name) match {
    case None => None
    case Some((t, v)) => Some(v)
  }

  def declare(name : String, typeOf : JayType) : Environment =
    new Environment(mapping + (name -> (typeOf, Undef)))

  private def unsafeSet(name : String, value : JayValue) : Environment =
    new Environment(mapping + (name -> (value.getCorrectType, value)))

  def set(name : String, value : JayValue) : Either[Environment, String] = retrieveType(name) match {
    case None => Right("Error! Variable of name " + name + " not declared")
    case Some(t) => (t, value) match {
      case (JBool, JayBool(_)) => Left(unsafeSet(name, value))
      case (JInt, JayInt(_)) => Left(unsafeSet(name, value))
      case pair => Right("Error! " + name + " is of type " + pair._1.toString +
        ", but the value to set is of type " + pair._2.getCorrectTypeName)
    }
  }

}


object Environment {
  def apply = new Environment(Map())
}

