package Lenguaje
import scala.collection.mutable.Map
/*
class Variable(var clave: String, var valor: Option[Evaluable]) extends Expresion

object Variable {

  def apply(clave: String): Variable = {
    var variable = new Variable(clave, None)
    Referencias.addVar(variable)
    return variable
  }

  def apply(clave: String, valor: Evaluable): Variable = {
    var variable = new Variable(clave, Some(valor))
    Referencias.addVar(variable)
    return variable
  }

  def unapply(variable: Variable): Option[(String, Option[Evaluable])] = Some(variable.clave, variable.valor)

}

case class Referencia(var clave: String) extends Evaluable {
  def getValue(): Evaluable = {
    var variable = Referencias.getVar(clave)
    return variable.valor.get
  }
}

//class Referencia(var referencias: Map[String, Variable]) extends Expresion

object Referencias extends Expresion {
  var referencias: Map[String, Variable] = Map()
  var varDuplicated: List[Variable] = List()

  def addVar(variable: Variable): Unit = {
    if (referencias.contains(variable.clave)) {
      varDuplicated = variable :: varDuplicated
    }
    referencias += (variable.clave -> variable)
  }

  def getVarOption(clave: String): Option[Variable] = {
      return referencias.get(clave)
  }

  def getVar(clave: String): Variable = {
    return referencias.get(clave).get
  }

  def reiniciar() = {
    referencias = Map()
    varDuplicated = List()
  }

  /*
  def apply(clave: String): Variable = this.getVar(clave).get

  def unapply(clave: String): Option[Variable] = this.getVar(clave)
  */
}

object Asignar {

  def apply(referencia:Referencia, valor:Evaluable): Variable = {
    val variable = Referencias.getVar(referencia.clave)
    variable.valor = Some(valor)
    return variable
  }
  
}

*/